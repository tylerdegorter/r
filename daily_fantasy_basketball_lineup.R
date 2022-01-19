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
#' @param dff_link: the link to the dff csv (figure out an API but do this in the meantime)
#' @param export: TRUE to export to CSV, FALSE to not
##############################################################################################################################

# Call Packages  ################################################################
library(tidyverse)
library(rvest)
library(fuzzyjoin)
library(reclin)
library(lpSolve)
library(lpSolveAPI)
library(gridExtra)

# Control Panel ################################################################
setwd("C:\\Users\\tyler\\OneDrive\\Desktop\\School\\Outside Class\\Daily Fantasy\\Projections\\")
max_salary = 50000
draftkings_url = "https://www.draftkings.com/lineup/getavailableplayerscsv?contestTypeId=70&draftGroupId=62230"
dff_link <- "C:\\Users\\tyler\\Downloads\\DFF_NBA_cheatsheet_2022-01-19.csv"

# Remove DFF file
# file.remove(dff_link)

# Build initial function
optimize_daily_fantasy_basketball <- function(max_salary = 50000, draftkings_url, dff_link, export = FALSE){
  
  ################################################################################
  # Load Data ####################################################################
  ################################################################################
  
  ##### Read in DraftKings data
  draftkings_data <- read_csv(draftkings_url) %>%
    rename(position = Position, name = Name, salary = Salary, roster_position = `Roster Position`, avg_pts_per_game = AvgPointsPerGame) %>%
    select(position, name, salary, roster_position, avg_pts_per_game) %>%
    mutate(name = gsub("`", "", name),
           name = gsub("'", "", name))
  
  ##### Read in Spotsline Projections
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
    rename(reb = TRB, blk = BK, stl = ST, Tm = TEAM, sl_ppg_projection = DK)
  names(projections_sportsline) <- tolower(names(projections_sportsline))
  
  ##### Read in Daily Fantasy Fuel Projections
  options(warn = -1) # suppress warnings
  dff_data <- read.csv(dff_link) %>%
    mutate(position = ifelse(!is.na(position_alt), paste0(position,"/",position_alt), position),
           name = paste0(first_name, " ", last_name)) %>%
    select(name, position, salary, L5_ppg_floor, L5_ppg_avg, L5_ppg_max, ppg_projection) %>%
    mutate(name = gsub("`", "", name),
           name = gsub("'", "", name)) %>%
    rename(dff_ppg_projection = ppg_projection)
  options(warn = 0) # turn warnings back on
  
  #####  Read in Basketball Reference data for 3pt pct
  html_document_br <- read_html("https://www.basketball-reference.com/leagues/NBA_2022_totals.html#totals_stats::fg3_pct")
  three_pt_pct <- html_table(html_document_br, fill = TRUE)[[1]] %>%
    filter(Player != 'Player') %>%
    group_by(Player) %>%
    mutate(three_pt_att = as.integer(ifelse(`3PA` == '', 0, `3PA`)),
           three_pt = as.integer(ifelse(`3P` == '', 0, `3P`)),
           FGA = as.integer(ifelse(FGA == '', 0, FGA))) %>%
    summarize(three_pt_att = sum(three_pt_att), three_pt = sum(three_pt), FGA = sum(FGA)) %>%
    mutate(three_pt_percent = ifelse(three_pt_att == 0, 0, three_pt / three_pt_att), 
           three_pt_apt_pct = ifelse(FGA == 0, 0, three_pt_att / FGA)) %>%
    select(Player, three_pt_percent, three_pt_apt_pct) %>%
    rename(player = Player) %>%
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
  
  ################################################################################
  # Data Transformation #########################################################
  ################################################################################
  
  #####  Bring in projected three points based on historical 3p % and projected attempted field goals
  projections_sportsline_b <- projections_sportsline %>%
    left_join(three_pt_pct, by = c("player")) %>%
    mutate(three_pt_made = fga * three_pt_percent * three_pt_apt_pct) %>%
    select(-fga, -three_pt_percent)
  
  #####  Build out the points projections by players
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
    select(player, sl_ppg_projection, fantasy_pts)
  
  #####  Combine with salary information
  combined_metrics <- draftkings_data %>%
    left_join(projections_sportsline_complete, by = c("name" = "player")) %>%
    left_join(select(dff_data, name, dff_ppg_projection, L5_ppg_floor, L5_ppg_max), by = c("name")) %>%
    mutate(
      is_PG = ifelse(grepl("PG", roster_position) == TRUE, 1, 0),
      is_SG = ifelse(grepl("SG", roster_position) == TRUE, 1, 0),
      is_SF = ifelse(grepl("SF", roster_position) == TRUE, 1, 0),
      is_PF = ifelse(grepl("PF", roster_position) == TRUE, 1, 0),
      is_C = ifelse(grepl("C", roster_position) == TRUE, 1, 0),
      is_G = ifelse(grepl("G", roster_position) == TRUE, 1, 0),
      is_F = ifelse(grepl("F", roster_position) == TRUE, 1, 0),
      is_G_F = ifelse(grepl("F", roster_position) == TRUE | grepl("G", roster_position) == TRUE, 1, 0),
      is_UTIL = ifelse(grepl("UTIL", roster_position) == TRUE, 1, 0)
    )
  
  ################################################################################
  # Optimization ################################################################
  ################################################################################
  
  ##### Build optimization function
  optimize_model <- function(fcst_source){
    
    #####  Filter out those without a projected score (likely injured / not playing)
    if (fcst_source == 'sl'){
      combined_metrics_elig <- filter(combined_metrics, sl_ppg_projection > 0)
      combined_metrics_elig$ppg_projection <- combined_metrics_elig$sl_ppg_projection
    } else if (fcst_source == 'dff'){
      combined_metrics_elig <- filter(combined_metrics, dff_ppg_projection > 0)
      combined_metrics_elig$ppg_projection <- combined_metrics_elig$dff_ppg_projection
    } else if (fcst_source == 'dff_min'){
      combined_metrics_elig <- filter(combined_metrics, dff_ppg_projection > 0)
      combined_metrics_elig$ppg_projection <- combined_metrics_elig$L5_ppg_floor
    } else if (fcst_source == 'dff_max'){
      combined_metrics_elig <- filter(combined_metrics, dff_ppg_projection > 0)
      combined_metrics_elig$ppg_projection <- combined_metrics_elig$L5_ppg_max
    } else if (fcst_source == 'all'){
      combined_metrics_elig <- filter(combined_metrics, dff_ppg_projection > 0)
      combined_metrics_elig$ppg_projection <- (combined_metrics_elig$sl_ppg_projection + combined_metrics_elig$dff_ppg_projection) / 2
    } 
    
    #####  Build initial base model based on # of variables
    num_decision_variables <- nrow(combined_metrics_elig)
    lprec <- make.lp(nrow = 0, ncol = num_decision_variables)
    
    #####  Set function to maximize
    lp.control(lprec, sense = "max")
    
    ##### Define objective function and constraints
    
    ##### For the model above, bring it in with the values to maximize
    set.objfn(lprec, combined_metrics_elig$ppg_projection)
    
    ##### Set constraints
    # Set salary constraint
    add.constraint(lprec, combined_metrics_elig$salary, "<=", max_salary)
    # Position constraint
    add.constraint(lprec, combined_metrics_elig$is_PG, ">=", 1) # need 1+ PG
    add.constraint(lprec, combined_metrics_elig$is_SG, ">=", 1) # need 1+ SG
    add.constraint(lprec, combined_metrics_elig$is_SF, ">=", 1) # need 1+ SF
    add.constraint(lprec, combined_metrics_elig$is_PF, ">=", 1) # need 1+ PF
    add.constraint(lprec, combined_metrics_elig$is_C, ">=", 1) # need 1+ C
    add.constraint(lprec, combined_metrics_elig$is_G, ">=", 3) # need 3+ Guards (PG, SG, G)
    add.constraint(lprec, combined_metrics_elig$is_F, ">=", 3) # need 3+ Forwards (SF, PF, F)
    add.constraint(lprec, combined_metrics_elig$is_G_F, ">=", 6) # need 6+ Guards / Fwds (PG, SG, SF, PF, G, F)
    add.constraint(lprec, combined_metrics_elig$is_UTIL, "=", 8) # need 8 players overall
    # Set to binary variables
    set.type(lprec, columns = c(1:num_decision_variables), type = 'binary')
    
    ##### Solve the model
    solve(lprec)
    
    ##### Get output
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
        mutate(fcst_model = fcst_source, date = Sys.Date()) %>%
        select(date, fcst_model, name, ppg_projection, avg_pts_per_game, 
               position, roster_position, salary),
      
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
          constraint = c("salary", "Num PG", "Num SG", "Num SF", "Num PF", "Num C", "Num G", "Num F", "Num G or F", "Num Util"),
          final_value = get.constraints(lprec),
          constr_type = get.constr.type(lprec),
          #rhs = get.rhs(lprec),
          constrained_value = get.constr.value(lprec)
        ),
      
      # Predicted vs Salary
      all_players_chart = combined_metrics_elig_w_select %>%
        ggplot(aes(x = salary, y = ppg_projection, color = as.factor(select))) +
        geom_point() +
        geom_smooth(method = 'lm', formula= y~x) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(y = 'Predicted Fantasy Points', x = 'Salary', title = "Projected Points vs Salary") +
        scale_color_manual(values=c("#c74444", "#0d903e")) +
        geom_text(data = filter(combined_metrics_elig_w_select, select == 1), 
                  aes(label = name), vjust = -2, check_overlap = TRUE, size = 3) +
        ylim(0, max(combined_metrics_elig_w_select$ppg_projection) * 1.2),
      
      # Predicted v Historical Actuals
      predicted_v_actual_chart = combined_metrics_elig_w_select %>%
        ggplot(aes(x = avg_pts_per_game, y = ppg_projection, color = as.factor(select))) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, size = 0.5) +
        theme_minimal() +
        theme(legend.position = "none", axis.title.y = element_blank()) +
        labs(x = 'Historical Averague', title = "Projected Points vs Historical Avg") +
        scale_color_manual(values=c("#c74444", "#0d903e")) +
        geom_text(data = filter(combined_metrics_elig_w_select, select == 1), 
                  aes(label = name), vjust = -2, check_overlap = TRUE, size = 3) +
        ylim(0, max(combined_metrics_elig_w_select$ppg_projection) * 1.2)
    )
    
  }
  
  final_output <- list(
    
    # Add in list of metrics
    all_player_stats = combined_metrics %>%
      filter(!is.na(sl_ppg_projection)) %>%
      mutate(date = Sys.Date()) %>%
      select(date, position, name, salary, roster_position, avg_pts_per_game, 
             sl_ppg_projection, dff_ppg_projection, L5_ppg_floor, L5_ppg_max),
    
    # Run model for SL data only
    sl_projection = optimize_model(fcst_source = "sl"),
    
    # Run model for DFF data only
    dff_projection = optimize_model(fcst_source = "dff"),
    
    # Run model for DFF min (minimize downside)
    dff_projection_min = optimize_model(fcst_source = "dff_min"),
    
    # Run model for DFF max (look for boom)
    dff_projection_max = optimize_model(fcst_source = "dff_max"),
    
    # Run model for Combined data
    all_projection = optimize_model(fcst_source = "all")
  )
  
  # Create file to be exported
  export_file <- rbind(final_output[[2]][[1]], 
                       final_output[[3]][[1]], 
                       final_output[[4]][[1]],
                       final_output[[5]][[1]],
                       final_output[[6]][[1]])
  
  # Export to csv?
  if(export == TRUE){
    
    # Export the file
    write_csv(x = export_file,
              file = paste0(getwd(),"\\projections_export_", 
                     gsub("-","",Sys.Date()), "_basketball.csv"))
    
    write_csv(x = final_output[[1]],
              file = paste0(getwd(),"\\all_stats_", 
                     gsub("-","",Sys.Date()), "_basketball.csv"))
  }
  
  # Append combined exported file
  final_output[[7]] <- export_file
  
  # Return combined list
  return(final_output)
  
  }

# Run model
output <- optimize_daily_fantasy_basketball(max_salary, draftkings_url, dff_link, export = TRUE)
