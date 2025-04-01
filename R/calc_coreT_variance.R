#' Calculate the temperature preference
#'
#' This function calculates the temperature preference for the trial
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param variance_type The method used for calculating the variance of core body temperature ("std_error", "std_deviation", "coeff_variation"). Default is "std_error".
#' @param exclude_acclimation Exclude the acclimation period from variable calculation, default = F
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @return variance in core body temperature
#' @export

calc_coreT_variance <- function(data, variance_type = c("std_error", "std_deviation", "coeff_variation"), exclude_start_minutes = 0, exclude_end_minutes = 0, exclude_acclimation = F) {
  # Ensure the body core temperature column exists
  if (!"core_T" %in% colnames(data)) {
    stop("The dataset does not contain a 'core_T' column. Please run file_prepare and calc_coreT")
  }
  
  # Convert exclude minutes to seconds
  exclude_start_seconds <- exclude_start_minutes * 60
  exclude_end_seconds <- exclude_end_minutes * 60
  
  if (exclude_acclimation) {
    data <- data[data$trial_phase != "acclimation", ]
  }
  
  # Exclude initial and final data if necessary
  start_time <- exclude_start_seconds
  end_time <- max(data$time_sec) - exclude_end_seconds
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  # Select the variance type
  variance_type <- match.arg(variance_type)
  
  # Calculate the variance measure
  if (variance_type == "std_error") {
    variance <- sd(data$core_T, na.rm = TRUE) / sqrt(length(na.omit(data$core_T)))
  } else if (variance_type == "std_deviation") {
    variance <- sd(data$core_T, na.rm = TRUE)
  } else if (variance_type == "coeff_variation") {
    variance <- sd(data$core_T, na.rm = TRUE) / mean(data$core_T, na.rm = TRUE)
  }
  
  return(variance)
}
