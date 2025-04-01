#' Plot the cumulative distance during the trial
#'
#' This function plots the cumulative distance covered over time
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @import ggplot2
#' @return Plot of the cumulative distance over time
#' @export

plot_distance <- function(data, exclude_start_minutes = 0, exclude_end_minutes = 0) {
  if (!("time_sec" %in% colnames(data) && "distance" %in% colnames(data))) {
    stop("The dataset does not contain 'time_sec' and/or 'distance' columns.")
  }
  
  # Convert time_sec to numeric if not already
  data$time_sec <- as.numeric(data$time_sec)
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  # Convert time in seconds to minutes
  data$Time_min <- data$time_sec / 60
  #substract first distance value to reset cumulative distance to 0
  data$distance<-data$distance-data$distance[[1]]
  
  # Plot cumulative distance
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = Time_min, y = (distance))) +
    ggplot2::geom_line(color = 'orange') +
    ggplot2::labs(title = "Cumulative Distance Moved During Trial",
                  x = "Time (minutes)",
                  y = "Distance Moved (cm)") +
    ggplot2::theme_light()
  
  print(plot)
}
