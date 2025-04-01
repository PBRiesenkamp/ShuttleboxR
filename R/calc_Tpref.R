#' Calculate the temperature preference
#'
#' This function calculates the temperature preference for the trial
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param method The method used for calculation of temperature preference ("median", "mean", "mode"). Default is "median".
#' @param exclude_acclimation Exclude the acclimation period from variable calculation, default = F
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @param print_results Print the results, default is TRUE
#' @return the temperature preference
#' @export

calc_Tpref <- function(data, method = c("median", "mean", "mode"), exclude_acclimation = F, exclude_start_minutes = 0, exclude_end_minutes = 0, print_results = T) {
  method <- match.arg(method)
  
  # Convert the time to numeric if not already
  data$time_sec <- as.numeric(data$time_sec)
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  if (exclude_acclimation) {
    data <- data[data$trial_phase != "acclimation", ]
  }
  
  # Convert the core_T column to numeric
  data$core_T <- as.numeric(data$core_T)
  
  # Calculate Tpref based on the specified method
  if (method == "median") {
    Tpref <- median(data$core_T, na.rm = TRUE)
  } else if (method == "mean") {
    Tpref <- mean(data$core_T, na.rm = TRUE)
  } else if (method == "mode") {
    Tpref <- as.numeric(names(sort(table(data$core_T), decreasing = TRUE))[1])
  }
  
  if (print_results) {
    print(paste("Tpref:", Tpref))
  }
  return(Tpref)
  
}
