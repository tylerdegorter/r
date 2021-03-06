##############################################################################################################################
#' This file provides a user defied function for using Facebook's Prophet package (https://facebook.github.io/prophet/). Given
#'   that prophet has many input parameters that we may not need, the function takes 2 inputs: a data frame and the number of
#'   periods to forecast. The default holiday file is used, only using US, CN, and IN as holiday countries since it reduces 
#'   processing time and covers most applicable holidays anyways. If we feed the function multiple columns to forecast, 
#'   it subsets those dimensions and loops through each individually to generate a data frame of forecasts for all outputs.
#' 
#' TODO (Tyler): The current output is a data frame of forecasts. Change the output into a list with the forecasts being one list
#'   element, and the intervals and holiday effects being another list element.
#' TODO (Tyler): Consider providing more customized lower and upper bounds for holidays
#'
#' @param df: the input data frame to pass into prophet. Must have a date column called "date" and a metric to forecast
#'   called "metric"; all other added dimensions are optional.
#' @param num_periods: the number of days to forecast after the end of the training data. Can set to a specific date using:
#'   as.Date('YYYY-MM-DD') - max(df$date)
##############################################################################################################################

# Call packages
suppressMessages(
  require(prophet),
  require(tidyverse)
)

# Create function
ProphetForecast <- function(df, num_periods, include_hist = FALSE) {
  
  ### Checks to make sure the column names are correct.
  date_check = sum(names(df) %in% 'date')
  metric_check = sum(names(df) %in% 'metric')
  
  # If "date" and "metric" are not columns, the script breaks and throws an error indicating what went wrong.
  if (date_check == 0 & metric_check == 0) {
    stop("No columns named 'date' or 'metric'; rename to those and try again")
  } else if (date_check == 0) {
    stop("No column named 'date'; rename and try again")
  } else if (metric_check == 0) {
    stop("No column named 'metric'; rename and try again")
  }
  
  ### Holidays
  # Creates the holiday dataset using "generated_holidays" data frame from the package. Since each holiday needs a lower and 
  #   upper range, we default to using -1 and 5 for now as it likely covers most holidays, although some may behave differently
  #   (ie. Christmas, Chinese New Year, Diwali). 
  # TODO(Tyler): Consider changing it so that the pre & post days for each holiday are specific to that holiday instead of 
  #   being used to cover a wide range.
  holiday_file <- generated_holidays %>%
    # change ds to date, and add window range (blunt now, can update to specific holidays)
    mutate(ds = as.Date(ds), 
           lower_window = -1, 
           upper_window = 5, 
           country = as.character(country)) %>% 
    # only grab the main countries to limit processing time
    filter(ds >= min(df$date) &
             ds <= as.Date(max(df$date)) + num_periods & 
             country %in% c("US", "CN", "IN")
    ) %>%
    select(-year, -country) %>%
    unique()
  
  ### Re-order columns
  # find where date and metric columns are in the input
  date_col <- names(df) %in% c("date")
  metric_col <- names(df) %in% c("metric")
  
  # Arrange in the date, then metric, then everything else. This is so the subsetting knows which columns to return.
  col_names <- c(names(df)[date_col], 
                 names(df)[metric_col],
                 names(df)[!(date_col | metric_col)])
  
  # Re-order based on the designated ordering above so the script runs properly.
  df <- df[, col_names]
  
  ### Get unique values to loop by and loop
  num_columns <- length(names(df))
  unique_dims <- df[, (3:num_columns)] %>% unique() %>% as.data.frame()
  output_df <- data.frame()
  
  # get start time before any loops to determine how long things take
  overall_start_time <- Sys.time()
  
  # Build loop from 1 to number of unique dimensions to forecast by
  for (i in 1:nrow(unique_dims)) {
    
    # get loop start time to determine length of this loop
    loop_start_time <- Sys.time()
    
    # Create temp table to subset from the main table from for the purpose of each row of the outer loop.
    df_temp <- df
    
    # Only return the values from the temp table that have to do with that set of unique values. Essentially, starting
    #   a the third column (first one after date & metric), return all values that show up in the third row of the 
    #   unique dims table. Continue with the next column and so on until we've fully subsetted down the data table.
    # TODO (Tyler): consider changing to an inner join to increase processing speed. 
    for (j in 3:num_columns) {
      df_temp <- df_temp[df_temp[, j] == unique_dims[i, j - 2], ]
    }
    
    # Change column names so it works with prophet, since prophet needs "ds" not "date", and "y" not "metric"
    names(df_temp)[1:2] <- c("ds", "y")
    
    # Run the forecast. First build the model, passing in the subsetted temp file and holiday file, and then
    #   run the prediction, which uses the model and number of periods to forecast. Only return the date and
    #   predicted value "yhat". Set uncertainty.samples to 0 to avoid prophet sampling the distribution - this
    #   reduces the run time by ~75% and isn't necessary since we aren't returning any intervals, just point
    #   estimates. 
    model <- df_temp %>% 
      prophet(holidays = holiday_file, uncertainty.samples = 0) %>% 
      suppressMessages()
    
    forecast <- predict(model, 
                        make_future_dataframe(model, 
                                              periods = num_periods))[, c("ds", "yhat")]
    
    # Plots components of the model - commenting out for now, consider putting back as a list element. Given
    #   we have multiple iterations of values we're forecasting, this may not be relevant now but keep it here
    # prophet_plots <- prophet_plot_components(model, forecast)
    
    # Append other columns to the data. After running the forecast, we only have a date and a number. Here
    #   we bring back in the other dimensions used to subset this particular loop of the time series and 
    #   add those columns back. 
    for (j in 3:num_columns){
      forecast <- cbind(forecast, unique_dims[i, j - 2])
    }
    names(forecast) <- names(df_temp)
    output_df <- rbind(output_df, forecast)
    
    # Get loop end time to determine length of this loop
    loop_end_time <- Sys.time()
    
    # So we know how long things take, print the loop we are on, as well as how many there are to go.
    writeLines(
      c(
        paste('done with loop', i, 'of', nrow(unique_dims)),
        paste('loop', i, 'took', format(loop_end_time - loop_start_time)),
        paste('estimated finish time:', 
              overall_start_time + ((loop_end_time - overall_start_time) * (nrow(unique_dims) / i))
        )
      )
    )
    cat("\n")
    
  }
  
  # Rename the output data frame with the same column names as the input data frame
  names(output_df) <- names(df)
  
  # Only keep the dates that start after the end of the input data.
  output_df_fcst_only <- output_df %>%
    filter(date > max(df$date))
  
  # If we want to include history, append it to the data below. Otherwise only return forecast output
  if (include_hist == TRUE) {
    output_df %>%
      filter(date > max(df$date)) %>% # only keep forecasted values, removed modeled historicals
      rbind(df) %>% # union the data
      mutate(flag = if_else(date > max(df$date), 'Forecast', 'History')) %>% # add a flag for history & forecast
      return()
  } else {
    output_df %>%
      filter(date > max(df$date)) %>% # just keep forecast
      return()
  }
  
}
