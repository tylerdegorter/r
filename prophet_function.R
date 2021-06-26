# Call package
require(prophet)

# Can use dataframe "generated_holidays" to use prophet default holidays
prophet_forecast <- function(df, num_periods){
  
  ### checks to make sure the column names are correct
  date_check = sum(names(df) %in% 'date')
  metric_check = sum(names(df) %in% 'metric')
  
  if(date_check == 0 & metric_check == 0){
    stop("No columns named 'date' or 'metric'; rename to those and try again")
  } else if (date_check == 0){
    stop("No column named 'date'; rename and try again")
  } else if (metric_check == 0){
    stop("No column named 'metric'; rename and try again")
  }
  
  # create holidays
  holiday_file <- generated_holidays %>%
    mutate(ds = as.Date(ds), lower_window = -1, upper_window = 5) %>% # change ds to date, and add window range (blunt now, can update to specific holidays)
    filter(ds >= min(input_data_a_s$ds) & ds <= max(input_data_a_s$ds) & country %in% c("US", "CN", "IN")) # only grab the main countries to limit processing time
  
  ### re-order columns
  # find where date and metric columns are in the input
  date_col <- names(df) %in% c("date")
  metric_col <- names(df) %in% c("metric")
  
  # arrange in the date, then metric, then everything else
  col_names <- c(names(df)[date_col], 
                 names(df)[metric_col],
                 names(df)[!(date_col | metric_col)])
  
  # re-order
  df <- df[,col_names]
  
  ### get unique values to loop by
  num_columns <- length(names(df))
  unique_dims <- unique(df[,(3:num_columns)])
  output_df <- data.frame()
  
  
  for (i in 1:nrow(unique_dims)){
    
    df_temp <- df
    
    # filter for each combination of rows
    for (j in 3:num_columns){
      df_temp <- df_temp[df_temp[,j] == unique_dims[i,j-2],]
    }
    
    # change column names so it works with prophet
    names(df_temp)[1:2] <- c("ds", "y")
    
    # forecast
    model <- prophet(df_temp, 
                     holidays = holiday_file)
    forecast <- predict(model, 
                        make_future_dataframe(model, 
                                              periods = num_periods))[,c("ds", "yhat")]
    
    # Comment out for now: plots components of the model
    # prophet_plot_components(model, forecast)
    
    # Append other columns to the data
    for (j in 3:num_columns){
      forecast <- cbind(forecast, unique_dims[i,j-2])
    }
    names(forecast) <- names(df_temp)
    output_df <- rbind(output_df, forecast)
    
    
    print(paste('running loop', i, 'of', nrow(unique_dims)))
    
  }
  
  names(output_df) <- names(df)
  output_df_fcst_only <- output_df %>%
    filter(date >= max(df$date))
  
  return(output_df_fcst_only)
  
}