#' Calculate the total distance
#'
#' This function calculates the total distance covered during the trial
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param exclude_acclimation Exclude the acclimation period from variable calculation, default = F
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @param print_results Print the results, default is TRUE
#' @return the total distance covered during the trial
#' @export

calc_tot_distance <- function(data, exclude_start_minutes = 0, exclude_end_minutes = 0, exclude_acclimation = FALSE, print_results = TRUE) {
  # Ensure the Distance moved column exists
  if (!"distance" %in% colnames(data)) {
    stop("The dataset does not contain a 'distance' column")
  }
  
  # Convert exclude minutes to seconds
  # Exclude initial and final data if necessary
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec) - exclude_end_minutes * 60
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  if (exclude_acclimation) {
    data <- data[data$trial_phase != "acclimation", ]
  }
  
  initial_distance <- data$distance[1]
  
  data$distance <- data$distance - initial_distance
  
  # Calculate net distance moved
  total_distance <- max(data$distance, na.rm = TRUE)
  
  # Print the result
  if (print_results) {
    print(paste("Distance Moved:", total_distance, "cm"))
  }
  
  return(total_distance)
}
