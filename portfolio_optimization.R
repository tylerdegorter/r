##############################################################################################################################
#' This file provides a user defied function for incorporating several stocks and returning an output that determines the
#'    weights across the stocks to maximize the Sharpe ratio. The sharpe ratio and portfolio weights are returned, as well 
#'    as the corresponding plot of the efficient frontier and tangency portfolio. Sharpe ratio components are as follows:
#'    
#'    Expected Return: calculated using Yahoo Finance 1-year analyst estimate divided by today's price
#'    Standard Deviation: Calculated using historical monthly returns per stock, up to 5 years
#'    Risk-Free Rate: Currently an input, but will change into a dynamic value
#'
#' @param tickers_input: a vector of stock tickers to evaluate as a portfolio
#' @param n_simul: number of simulations to run
#' @param rf_rate: the risk-free rate to be used; assume 0. Plan is to make dynamic
##############################################################################################################################

# Call packages

require(tidyverse)
require(xml2)
require(quantmod)
require(lubridate)
require(htmltab)

calculateOptimalPortfolio <- function(tickers_input, n_simul = 10000){
  
  ######################## Set up datasets to use later
  
  # get list of stock tickers to use later
  tickers <- sort(tickers_input)
  
  # create a matrix to hold calculated values
  target_estimates <- matrix(data = 0, nrow = 5, ncol = length(tickers))
  colnames(target_estimates) <- tickers
  rownames(target_estimates) <- c("current_price", "target_price", "expected_return", "stdev_return", 'dividend')
  
  ######################## Get historical stock prices
  
  # Query data from Google Finance
  getSymbols(tickers,
             from = (Sys.Date() - (5 * 365)), 
             to = Sys.Date(),
             warnings = FALSE,
             auto.assign = TRUE)
  
  # Grab the 1 month t bill rate from the treasury yield curve
  rf_rate <- "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/pages/textview.aspx?data=yield" %>%
    htmltab(which = 2, header = 1) %>%
    suppressWarnings() %>%
    select(`1 mo`) %>%
    tail(1) %>%
    mutate(rate = as.numeric(`1 mo`) / 100) %>%
    pull(rate)
  
  # get historical time series and compile
  historical_prices <- map(tickers, function(x) Ad(get(x)))
  historical_prices <- reduce(historical_prices, merge)
  colnames(historical_prices) <- tickers
  current_prices <- tail(historical_prices, 1)
  
  # add in month, year to the time series
  historical_prices_df <- as.data.frame(historical_prices)
  historical_prices_df$date <- rownames(historical_prices_df)
  historical_prices_df$month <- month(historical_prices)
  historical_prices_df$year <- year(historical_prices)
  
  # calculate the first trading day of each month
  join_dates <- historical_prices_df %>%
    group_by(year, month) %>%
    summarize(date = min(date))
  
  # get another data frame of monthly prices only
  historical_prices_df_monthly <- historical_prices_df %>%
    inner_join(join_dates, by = c("year", "month", "date"))
  
  # get data frame of monthly returns
  historical_returns <- historical_prices_df_monthly %>%
    gather(key = 'ticker', value = 'price', -month, -year, -date) %>%
    mutate(price_last_month = lag(price, 1)) %>%
    filter(is.na(price_last_month) == 0) %>%
    mutate(return = price / price_last_month - 1)
  
  # get standard deviation of returns
  historical_stdev_returns <- historical_returns %>%
    group_by(ticker) %>%
    summarize(stdev_return = sd(return))
  
  # Calculate variance covariance matrix
  VCV <- historical_returns %>%
    select(date, ticker, return) %>%
    spread(key = 'ticker', value = 'return', fill = 0) %>%
    select(-date) %>%
    cov()
  
  ######################## Build matrix of returns, stdev, and returns by stock
  
  for (i in 1:length(tickers)){
    
    ## Pass current stock prices
    target_estimates[1,i] <- current_prices[1,i]
    
    ## Get analyst target estimates
    scraped_data <- paste0("https://finance.yahoo.com/quote/",tickers[i],"?p=",tickers[i]) %>%
      htmltab(which = 2, header = 0) %>%
      suppressWarnings()
    
    target_price <- scraped_data[8,2]
    
    dividend <- substr(scraped_data[6, 2], 1, gregexpr(" ", scraped_data[6, 2])[[1]][1] - 1) %>%
      as.numeric()
    
    # dividend
    target_estimates[5,i] <- ifelse(is.na(dividend), 0, dividend)
    
    # target price
    target_estimates[2,i] <- gsub(",","",target_price) %>% 
      as.numeric()
    
    # expected return
    target_estimates[3,i] <- (target_estimates[2,i] + target_estimates[5,i]) / target_estimates[1,i] - 1
    
    # standard deviation
    target_estimates[4,i] <- historical_stdev_returns %>%
      filter(ticker == tickers[i]) %>%
      pull(stdev_return)
    
    paste0("Done with stock ", i, " of ", length(tickers)) %>%
      print()
    
  }
  
  # convert to a data frame
  target_estimates_df <- t(target_estimates) %>% 
    as.data.frame()
  target_estimates_df$ticker <- rownames(target_estimates_df)
  rownames(target_estimates_df) <- NULL
  
  
  ######################## Incorporate weights for each stock through simulation
  
  # Create simulation matrix to be used later
  simul_matrix <- matrix(data = 0, nrow = n_simul, ncol = length(tickers) + 2)
  colnames(simul_matrix) <- c("return", "vol", paste0(tickers, "_wt"))
  
  # Run simulation 
  for (j in 1:n_simul){
    
    # randomly select weights and ensure they sum to 1
    rand_weights <- rnorm(length(tickers), mean = 1 / length(tickers), sd = 1 / length(tickers) * 0.5)
    rand_weights <- rand_weights / sum(rand_weights)
    
    # calculate portfolio return and standard deviation
    simul_matrix[j, 1] <- target_estimates_df$expected_return %*% t(t(rand_weights))
    simul_matrix[j, 2] <- (rand_weights %*% VCV %*% t(t(rand_weights))) %>% sqrt()
    simul_matrix[j, 3:ncol(simul_matrix)] <- rand_weights
    
  }
  
  # Convert the matrix to a data frame
  simul_df <- simul_matrix %>%
    as.data.frame() %>%
    mutate(sharpe = (return - rf_rate) / vol)
  
  # Return the optimal portfolio based on simulations
  optim_portfolio <- simul_df %>% arrange(desc(sharpe)) %>% head(1)
  
  ######################## Charting
  
  # Calculate the tangency portfolio based on inputs used above
  tangency <- data.frame(seq(0, optim_portfolio$return * 2, 0.001))
  colnames(tangency) <- c("vol")
  tangency <- tangency %>%
    as.data.frame() %>%
    mutate(ret = rf_rate + optim_portfolio$sharpe * vol)
  
  # Plot the efficient frontier and tangency portfolios
  final_ggplot_chart <- simul_df %>%
    ggplot(aes(x = vol, y = return)) +
    geom_point() +
    xlim(0, optim_portfolio$vol * 2) +
    ylim(rf_rate * 0.8, optim_portfolio$return * 1.5) +
    geom_line(data = tangency, aes(x = vol, y = ret, color = 'red', linemitre = 2))
  
  # compile optimal portfolio and chart into a list and export it
  output <- list(optim_portfolio, target_estimates_df, VCV, final_ggplot_chart)
  return(output)
}

# Runs the function using sample portfolio
calculateOptimalPortfolio(c("PYPL", "WMT", "VZ", "LMT"), n_simul = 1000000)
