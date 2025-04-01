#' Calculate the gravitation period
#'
#' This function calculates the gravitation period using a segmented regression with a single breakpoint. The breakpoint represents the moment where temperature preference was reached. The time until that point is the gravitation time
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param exclude_acclimation Exclude the acclimation period from variable calculation, default = F
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @param print_results Print the results, default is TRUE
#' @import segmented
#' @return the temperature preference
#' @export


calc_gravitation <- function(data, exclude_start_minutes = 0, exclude_end_minutes = 0, exclude_acclimation = F, print_results = T) {
  
  # Convert the time to numeric if not already
  data$time_sec <- as.numeric(data$time_sec)
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  if (exclude_acclimation) {
    data <- data[data$trial_phase != "acclimation", ]
  }
  
  # Perform segmented regression
  fit <- lm(core_T ~ time_h, data = data)
  seg_fit <- segmented::segmented(fit, seg.Z = ~time_h, npsi = 1)
  
  # Get summary and breakpoint
  seg_summary <- summary(seg_fit)
  breakpoints <- seg_fit$psi[, "Est."]
  
  # Extract breakpoints
  breakpoints <- seg_fit$psi[, "Est."]
  
  if (length(breakpoints) == 0) {
    stop("No breakpoints found in the segmented model.")
  }
  
  # Assume the first breakpoint as the gravitation time
  gravitation_time <- breakpoints[1]-min(data$time_h)
  
  if (print_results) {
    print(paste("Gravitation Time (hours):", gravitation_time))
  }
  
  return(gravitation_time)
}
