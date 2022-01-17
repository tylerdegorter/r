##############################################################################################################################
#' This file provides a process to calculate optimal Daily Fantasy lineups for Basketball. It takes data from DraftKings for
#'   salaries and sportsline.com for NBA stat projections for the current day. It then projects the number of points per player
#'   based on DraftKings scoring, combines it with the salary data, and runs a linear optimization model to pick a team that
#'   maximizes points based on constraints, which are listed below:
#'
#' Constraints:
#'   Salary: salary must be under 50000
#'   Positions: the number of PG, SG, SF, PF, and C must adhere to DraftKings. In addition, general G, F, and UTIL are accouned for
#' 
#' TODO(tyler): Consider adding historical gamelogs to identify variance of player stats. This allows for Monte Carlo simulation
#'   with the intent of analyzing distribution of potential scores for optimal outcomes as well as which players show up the most
#'   often in the optimal team.
#'
#' @param max_salary: the maximum salary that can be spent. Defaults to 50,000 as that is the DraftKings figure
#' @param draftkings_url: the url of the salary data from DraftKings. Go into a lobby and right click "export" and copy the link
#'   to paste here
##############################################################################################################################

# Call Packages  ################################################################
library(tidyverse)
library(rvest)
library(fuzzyjoin)
library(reclin)
library(lpSolve)
library(lpSolveAPI)

# Control Panel ################################################################
max_salary = 50000
draftkings_url = "https://www.draftkings.com/lineup/getavailableplayerscsv?contestTypeId=70&draftGroupId=62384"

# Load Data ####################################################################
#
#

##### Read in DraftKings data
draftkings_data <- read_csv(draftkings_url) %>%
  rename(position = Position, name = Name, salary = Salary, roster_position = `Roster Position`) %>%
  select(position, name, salary, roster_position) %>%
  mutate(name = gsub("`", "", name),
         name = gsub("'", "", name))

##### Read in Projections
### Read in Spotsline Projections
html_document_sportsline <- read_html("https://www.sportsline.com/nba/expert-projections/simulation/")

projections_all_sportsline <- html_table(html_document_sportsline, fill = TRUE)[[1]]

projections_sportsline <- projections_all_sportsline %>%
  # only keep columns we want 
  select(PLAYER, TEAM, DK, PTS, AST, TRB, BK, ST, TO, FGA) %>%
  # change block from char to num
  mutate(PTS = ifelse(PTS == '-', 0, PTS),
         AST = ifelse(AST == '-', 0, AST),
         TRB = ifelse(TRB == '-', 0, TRB),
         BK = ifelse(BK == '-', 0, BK),
         ST = ifelse(ST == '-', 0, ST),
         TO = ifelse(TO == '-', 0, TO),
         FGA = ifelse(FGA == '-', 0, FGA),
         PTS = as.numeric(PTS),
         AST = as.numeric(AST),
         TRB = as.numeric(TRB),
         BK = as.numeric(BK),
         ST = as.numeric(ST),
         TO = as.numeric(TO),
         FGA = as.numeric(FGA),
         # Remove ' and ` from player names
         PLAYER = gsub("`", "", PLAYER),
         PLAYER = gsub("'", "", PLAYER)) %>%
  # rename for consistency
  rename(reb = TRB, blk = BK, stl = ST, Tm = TEAM)



names(projections_sportsline) <- tolower(names(projections_sportsline))

# Read in Basketball Reference data for 3pt pct
html_document_br <- read_html("https://www.basketball-reference.com/leagues/NBA_2022_totals.html#totals_stats::fg3_pct")

three_pt_pct <- html_table(html_document_br, fill = TRUE)[[1]] %>%
  filter(Player != 'Player') %>%
  mutate(three_pt_percent = ifelse(`3P%` == '', 0, `3P%`)) %>%
  mutate(three_pt_percent = as.numeric(three_pt_percent),
         three_pt_att = as.numeric(`3PA`),
         FGA = as.numeric(FGA)) %>%
  mutate(three_pt_apt_pct = three_pt_att / FGA) %>%
  select(Player, Tm, three_pt_percent, three_pt_apt_pct) %>%
  rename(player = Player, tm = Tm) %>%
  mutate(player = iconv(player, from="UTF-8", to="ASCII//TRANSLIT")) %>%
  mutate(player = gsub("`", "", player),
         player = gsub("'", "", player))

##### Create tables for scoring and positions
scoring <- data.frame(
  pts = 1,
  three_pts = 0.5,
  reb = 1.25,
  ast = 1.5,
  stl = 2,
  blk = 2,
  to = -0.5,
  dbl_dbl = 1.5,
  trpl_trpl = 3
)

positions <- data.frame(
  PG = 1,
  SG = 1,
  SF = 1,
  PF = 1,
  C = 1,
  G = 1,
  F = 1,
  UTIL = 1
)


##### Remove excess data frames
rm(html_document_sportsline, projections_all_sportsline, html_document_br)

# Data Transformation #########################################################
#
#

# Bring in projected three points based on historical 3p % and projected attempted field goals
projections_sportsline_b <- projections_sportsline %>%
  left_join(select(three_pt_pct, -tm), by = c("player")) %>%
  mutate(three_pt_made = fga * three_pt_percent * three_pt_apt_pct) %>%
  select(-fga, -three_pt_percent, -tm)

# Build out the points projections by players
projections_sportsline_complete <- projections_sportsline_b %>%
  mutate(dbl_digit_stats = ifelse(pts > 10, 1, 0) + 
           ifelse(ast > 10, 1, 0) + 
           ifelse(reb > 10, 1, 0) + 
           ifelse(blk > 10, 1, 0)) %>%
  mutate(fantasy_pts = pts * scoring$pts + 
           ast * scoring$ast + 
           reb * scoring$reb + 
           blk * scoring$blk + 
           stl * scoring$stl + 
           to * scoring$to + 
           three_pt_made * scoring$three_pts +
           ifelse(dbl_digit_stats == 2, scoring$dbl_dbl, 0) +
           ifelse(dbl_digit_stats >= 3, scoring$trpl_trpl, 0)) %>%
  select(player, dk, fantasy_pts)

# Combine with salary information
combined_metrics <- draftkings_data %>%
  left_join(projections_sportsline_complete, by = c("name" = "player")) %>%
  mutate(
    is_PG = ifelse(grepl("PG", roster_position) == TRUE, 1, 0),
    is_SG = ifelse(grepl("SG", roster_position) == TRUE, 1, 0),
    is_SF = ifelse(grepl("SF", roster_position) == TRUE, 1, 0),
    is_PF = ifelse(grepl("PF", roster_position) == TRUE, 1, 0),
    is_C = ifelse(grepl("C", roster_position) == TRUE, 1, 0),
    is_G = ifelse(grepl("G", roster_position) == TRUE, 1, 0),
    is_F = ifelse(grepl("F", roster_position) == TRUE, 1, 0),
    is_UTIL = ifelse(grepl("UTIL", roster_position) == TRUE, 1, 0)
  )


# Optimization ################################################################
#
#

### Filter out those without a projected score (likely injured / not playing)
combined_metrics_elig <- filter(combined_metrics, !is.na(dk))

### Build initial base model based on # of variables
num_decision_variables <- nrow(combined_metrics_elig)
lprec <- make.lp(nrow = 0, ncol = num_decision_variables)

### Set function to maximize
lp.control(lprec, sense="max")

### Define objective function and constraints

# for the model above, bring it in with the values to maximize
set.objfn(lprec, combined_metrics_elig$dk) 

### Set constraints

# Set salary constraint
add.constraint(lprec, combined_metrics_elig$salary, "<=", max_salary)
# Position constraint
add.constraint(lprec, combined_metrics_elig$is_PG, ">=", 1)
add.constraint(lprec, combined_metrics_elig$is_SG, ">=", 1)
add.constraint(lprec, combined_metrics_elig$is_SF, ">=", 1)
add.constraint(lprec, combined_metrics_elig$is_PF, ">=", 1)
add.constraint(lprec, combined_metrics_elig$is_C, ">=", 1)
add.constraint(lprec, combined_metrics_elig$is_G, ">=", 3)
add.constraint(lprec, combined_metrics_elig$is_F, ">=", 3)
add.constraint(lprec, combined_metrics_elig$is_UTIL, "=", 8)

# Set to binary variables
set.type(lprec, columns = c(1:num_decision_variables), type = 'binary')

### Solve the model
solve(lprec)

### Get output

# Optimized points
get.objective(lprec)

# Players to pick
get.variables(lprec)

# add in selection column to dataset
combined_metrics_elig_w_select = combined_metrics_elig %>%
  mutate(select = get.variables(lprec)) 

# Return players to pick
select_team <- list(
  
  # The roster and stats of the optimized team
  roster = combined_metrics_elig_w_select %>%
    filter(select == 1) %>%
    select(name, dk, position, roster_position, salary),
  
  # THe predicted points of the optimal team
  predicted_points = get.objective(lprec),
  
  # The salary of the optimal team
  salary = combined_metrics_elig %>%
    mutate(select = get.variables(lprec)) %>%
    filter(select == 1) %>%
    pull(salary) %>% 
    sum(),
  
  # The constraints 
  constraints = 
    data.frame(
      constraint = c("salary", "Num PG", "Num SG", "Num SF", "Num PF", "Num C", "Num G", "Num F", "Num Util"),
      final_value = get.constraints(lprec),
      constr_type = get.constr.type(lprec),
      #rhs = get.rhs(lprec),
      constrained_value = get.constr.value(lprec)
    ),
  
  # Visualization
  combined_metrics_elig_w_select %>%
    ggplot(aes(x = salary, y = dk, color = as.factor(select))) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    theme(legend.position = "none") +
    labs(y = 'Fantasy Points', x = 'Salary') +
    geom_text(data = filter(combined_metrics_elig_w_select, select == 1), aes(label = name), vjust = -2)
)

select_team
