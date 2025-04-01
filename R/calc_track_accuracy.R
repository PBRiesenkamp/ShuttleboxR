#' Calculate the tracking accuracy
#'
#' This function calculates the proportion of time the subject was tracked accurately during the trial
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param exclude_acclimation Exclude the acclimation period from variable calculation, default = F
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @param print_results Print the results, default is TRUE
#' @return the tracking accuracy
#' @export

calc_track_accuracy <- function (data, exclude_start_minutes = 0, exclude_end_minutes = 0, exclude_acclimation = F, print_results = T){
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  # Exclude acclimation if requested
  if (exclude_acclimation) {
    data <- data[data$trial_phase != "acclimation", ]
  }
  
  observations <- nrow(data)
  missed_coordinates <- sum(is.na(data$x_pos))
  
  prop <- 1-(missed_coordinates/observations)
  
  if (print_results) {
    print(paste0("Proportion of time where subject was tracked: ", prop*100, "%"))
  }
  return(prop)
  
}
