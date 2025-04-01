#' Prepare shuttle-box data
#'
#' This prepares shuttle-box data by adding the variables time_h and time_s (time in hours and time in seconds), correcting the timeseries in case the trial continues past midnight, and delimiting the acclimmation period using trial_start
#'
#' @param data a raw shuttle-box dataframe, for example as output by read_shuttlesoft
#' @return an organised shuttle-box dataframe, ready for calculation steps
#' @export

file_prepare <- function(data) {
  
  # Add time in seconds and hours assuming a sampling frequency of 1 Hz
  data$time_sec <- seq(0, by = 1, length.out = nrow(data))
  data$time_h <- data$time_sec / 3600
  
  if (all(c("x_pos", "y_pos") %in% colnames(data))) {
    
    data$x_pos[data$x_pos == "No object"] <- NA
    data$y_pos[data$y_pos == "No object"] <- NA
    
    data$x_pos <- as.numeric(data$x_pos)
    data$y_pos <- as.numeric(data$y_pos)
  }
  
  # Make sure the date is correct in case the experiment goes on past midnight
  # Extract the second when the experiment passed midnight
  # Make all observations past that date the next day
  if(length(unique(data$date)) == 1){
    
    midnight_observation <- data$time_sec[data$time == "00:00:00"]
    if (length(midnight_observation) > 0) {
      data$date[data$time_sec >= midnight_observation] <- data$date[data$time_sec >= midnight_observation] + 1
    }
  }
  
  # Create datetime object
  data$datetime<-as.POSIXct(paste(data$date, data$time), format = "%Y-%m-%d %H:%M:%S")
  
  # Add a column that tells you whether the system is in static or dynamic
  if ("delta_T" %in% colnames(data))  data$dyn_stat <- ifelse(is.na(data$delta_T), "static", "dynamic")
  
  # Add the phase of the experiment (e.g. acclimation and trial)
  trial_start_second<-data$time_sec[data$time == data$trial_start]
  data$trial_phase<-ifelse(data$time_sec>=trial_start_second, "trial", "acclimation")
  
  # Detect a shuttle (chamber side change) in new column
  data$shuttle <- 0
  for (i in 2:nrow(data)) {
    if (data$zone[i] != data$zone[i-1]) {
      data$shuttle[i] <- 1
    } else {
      data$shuttle[i] <- 0
    }
  }
  
  # Ensure the shuttle column is numeric
  data$shuttle <- as.numeric(data$shuttle)
  
  # Check if necessary columns exist in the data. If not, return NA for those columns
  
  if (!any(c("x_pos", "y_pos", "distance") %in% colnames(data))) {
    warning("The dataset does not contain a 'distance' column nor 'x_pos' and 'y_pos' to calculate 'distance', adding empty column")
    data$distance <- NA
  }
  
  if (!all(c("max_T", "min_T") %in% colnames(data))) {
    warning("The dataset does not contain 'min_T' and/or 'max_T' columns, adding empty columns")
    data$min_T <- NA
    data$max_T <- NA
  }
  
  data<-type.convert(data, as.is = T)
  data$datetime<-as.POSIXct(data$datetime)
  return(data)
}
