#' Plot the temperatures in each side of the shuttlebox over time
#'
#' This function plots the temperature in the warm and cold chamber during the trial
#'
#' @param data An organised shuttle-box dataframe
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @import ggplot2
#' @return a plot with the temperatures in the warm and cold chambers
#' @export


plot_T_gradient <- function(data, exclude_start_minutes = 0, exclude_end_minutes = 0) {
  # Convert time_sec to numeric if not already
  data$time_sec <- as.numeric(data$time_sec)
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  # Convert time in seconds to hours
  data$time_h <- data$time_sec / 3600
  
  # Plot the temperature gradient
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = time_h)) +
    ggplot2::geom_line(ggplot2::aes(y = `DECR_T`, color = "Cold chamber")) +
    ggplot2::geom_line(ggplot2::aes(y = `INCR_T`, color = "Warm chamber")) +
    ggplot2::scale_color_manual(values = c("Warm chamber" = "brown1", "Cold chamber" = "dodgerblue")) +
    ggplot2::labs(title = "Temperature Gradient Over Time",
                  x = "Time (hours)",
                  y = "Temperature (Celsius)",
                  color = "Temperature Side") +
    ggplot2::theme_light()
  
  print(plot)
}
