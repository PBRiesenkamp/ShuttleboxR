#' Calculate the number of shuttles
#'
#' This function calculates the total number of times the subject shuttled during the trial
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param exclude_acclimation Exclude the acclimation period from variable calculation, default = F
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @param print_results Print the results, default is TRUE
#' @return The total number of shuttles
#' @export

calc_shuttles <- function(data, exclude_start_minutes = 0, exclude_end_minutes = 0, exclude_acclimation = F, print_results = T) {
  # Ensure the 'shuttle' and 'time_sec' columns exist
  if (!all(c("shuttle", "time_sec") %in% colnames(data))) {
    stop("The dataset does not contain 'shuttle' and/or 'time_sec' columns. Run file_prepare on your data.")
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
  
  # Calculate shuttling frequency
  shuttles <- sum(data$shuttle, na.rm = TRUE)
  
  if (print_results) {
    # Print the result
    print(paste("Shuttling frequency:", shuttles))
  }
  
  return(shuttles)
}
