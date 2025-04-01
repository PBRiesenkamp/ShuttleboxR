#' Calculate the time spent near the extremes
#'
#' This function calculates the time the subject spent near the set minimum and maximum temperature limits during the shuttle-box trial
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param threshold Definition of the extreme temperature range. Default is 20% of the temperature range: 0.2*(max(data$max_T)-max(data$min_T))
#' @param exclude_acclimation Exclude the acclimation period from variable calculation, default = F
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @param print_results Print the results, default is TRUE
#' @return the time spent near each extreme
#' @export

calc_extremes <- function(data, threshold = 0.2*(max(data$max_T)-max(data$min_T)), exclude_start_minutes = 0, exclude_end_minutes = 0, exclude_acclimation = F, print_results = T) {
  # Ensure necessary columns exist
  if (!("time_sec" %in% colnames(data) && "core_T" %in% colnames(data))) {
    stop("The dataset does not contain 'time_sec' and/or 'core_T' columns.")
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
  
  # Convert time in secxonds to minutes
  data$Time_min <- data$time_sec / 60
  
  
  upper_limit<-max(data$max_T)
  lower_limit<-max(data$min_T)
  
  # Determine time spent near upper and lower extreme temperatures with the threshold
  upper_threshold <- upper_limit - threshold
  lower_threshold <- lower_limit + threshold
  data$Upper_Extreme_T <- ifelse(data$core_T > upper_threshold, 1, 0)
  data$Lower_Extreme_T <- ifelse(data$core_T < lower_threshold, 1, 0)
  
  time_near_upper_extreme <- sum(data$Upper_Extreme_T) / nrow(data) * 100
  time_near_lower_extreme <- sum(data$Lower_Extreme_T) / nrow(data) * 100
  
  if (print_results) {
    # Print the results
    print(paste("Time spent near upper extreme temperatures (%):", time_near_upper_extreme))
    print(paste("Time spent near lower extreme temperatures (%):", time_near_lower_extreme))
  }
  
  return(c(time_near_lower_extreme, time_near_upper_extreme))
}
