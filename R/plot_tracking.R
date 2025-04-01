#' Plot number of lost tracks per interval
#'
#' This function plots the number of missing tracks per interval
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param interval_minutes Specify the interval length in minutes to determine tracking frequency, default is 60
#' @param exclude_acclimation Exclude the acclimation period from variable calculation, default is TRUE
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @import ggplot2 
#' @return Plot of the number of missed tracks per interval
#' @export

plot_tracking <- function(data, interval_minutes = 60, exclude_start_minutes = 0, exclude_end_minutes = 0, exclude_acclimation = F) {
  
  # Ensure necessary columns exist
  if (!all(c("time_h", "x_pos", "y_pos") %in% colnames(data))) {
    stop("The dataset does not contain one or more necessary columns: 'time_h', 'x_pos', 'y_pos'.")
  }
  
  # Convert the time to numeric if not already
  data$time_sec <- as.numeric(data$time_sec)
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  # Exclude acclimation if requested
  if (exclude_acclimation) {
    data <- data[data$trial_phase != "acclimation", ]
  }
  
  data$mistrack <- NA
  
  for (i in seq_len(nrow(data))) {
    if (is.na(data$x_pos[i])) {
      data$mistrack[i] <- 1
    } else {
      data$mistrack[i] <- 0
    }
  }
  
  # Calculate time intervals
  data$t_interval <- floor((data$time_sec - start_time) / (interval_minutes * 60)) * interval_minutes
  
  # Calculate mean rate for each interval
  rate <- aggregate(data$mistrack ~ t_interval, data = data, FUN = function(x) sum(x, na.rm = TRUE))
  names(rate)[names(rate) == "data$mistrack"] <- "mistrack"
  
  rate$t_interval<-rate$t_interval/60
  
  # Convert Time_interval to numeric for plotting
  rate$t_interval <- as.numeric(rate$t_interval)
  
  # Plot the mean shuttling rate
  plot <- ggplot2::ggplot(rate, ggplot2::aes(x = t_interval, y = mistrack)) +
    ggplot2::geom_line(color = "black") +
    ggplot2::geom_point(color = "#F79518") +
    ggplot2::labs(title = "Number of missed tracks over the trial",
                  x = "Time (h)",
                  y = paste("Missed tracks/h "))+
    ggplot2::theme_light()
  
  print(rate)
  
  return(plot)
  
}
