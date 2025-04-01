#' Calculate the avoidance temperatures
#'
#' This function calculates the upper and lower avoidance temperature for the trial
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param percentiles The lower and upper percentile for lower and upper avoidance temperature calculation resp. Default is c(0.05, 0.95)
#' @param exclude_acclimation Exclude the acclimation period from variable calculation, default = F
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @param print_results Print the results, default is TRUE
#' @return the upper and lower avoidance temperature
#' @export

calc_Tavoid <- function(data, percentiles = c(0.05, 0.95), exclude_start_minutes = 0, exclude_end_minutes = 0, exclude_acclimation = F, print_results = T) {
  # Ensure the percentiles are valid
  if (length(percentiles) != 2 || any(percentiles < 0) || any(percentiles > 1)) {
    stop("Tavoid percentiles should be a vector of two values between 0 and 1")
  }
  
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  if (exclude_acclimation) {
    data <- data[data$trial_phase != "acclimation", ]
  }
  # Calculate the percentiles
  Tavoid_lower <- quantile(data$core_T, percentiles[1], na.rm = TRUE, names = F)
  Tavoid_upper <- quantile(data$core_T, percentiles[2], na.rm = TRUE, names = F)
  
  if (print_results) {
    # Print values
    print(paste("Tavoid Lower:", Tavoid_lower))
    print(paste("Tavoid Upper:", Tavoid_upper)) 
  }
  
  return(c(Tavoid_lower, Tavoid_upper))
  
}
