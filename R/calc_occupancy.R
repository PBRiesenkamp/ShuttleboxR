#' Calculate the occupancy times
#'
#' This function calculates the time spent in each chamber
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param exclude_acclimation Exclude the acclimation period from variable calculation, default = F
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @param print_results Print the results, default is TRUE
#' @return the occupancy times
#' @export


calc_occupancy <- function(data, exclude_start_minutes = 0, exclude_end_minutes = 0, exclude_acclimation = F, print_results = T) {
  # Ensure the 'zone' and 'time_sec' columns exist
  if (!all(c("zone", "time_sec") %in% colnames(data))) {
    stop("The dataset does not contain 'zone' and/or 'time_sec' columns.")
  }
  
  # Convert time_sec to numeric if not already
  data$time_sec <- as.numeric(data$time_sec)
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  if (exclude_acclimation) {
    data <- data[data$trial_phase != "acclimation", ]
  }
  
  # Calculate occupancy time in each chamber
  time_in_chamberDECR <- sum(data$zone == "DECR", na.rm = TRUE)
  time_in_chamberINCR <- sum(data$zone == "INCR", na.rm = TRUE)
  
  if (print_results) {
    # Print the results
    print(paste("Time in DECR Chamber:", time_in_chamberDECR))
    print(paste("Time in INCR Chamber:", time_in_chamberINCR))
  }
  
  return(c(time_in_chamberDECR, time_in_chamberINCR))
}
