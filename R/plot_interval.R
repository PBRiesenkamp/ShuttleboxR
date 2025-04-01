#' Plot the interval means for a selected column over time
#'
#' This function plots the  the interval means for a selected column over time
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param column The name of the column to plot an interval for
#' @param interval_minutes The number of minutes per interval, default is 10
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @import ggplot2
#' @return Plot of the interval means for a selected column over time
#' @export

plot_interval <- function(data, column, interval_minutes = 10, exclude_start_minutes = 0, exclude_end_minutes = 0) {
  
  # Convert time_sec to numeric if not already
  data$time_sec <- as.numeric(data$time_sec)
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec, na.rm = T) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  # Calculate time intervals
  data$t_interval <- floor((data$time_sec - start_time) / (interval_minutes * 60)) * interval_minutes
  
  # Calculate mean rate for each interval
  rate <- aggregate(data[[column]] ~ t_interval, data = data, FUN = function(x) mean(x, na.rm = TRUE))
  names(rate)[names(rate) == "data[[column]]"] <- column
  
  rate$t_interval<-rate$t_interval/60
  
  if (column == "shuttle") {rate[[column]] <- rate[[column]]*60}
  
  # Convert Time_interval to numeric for plotting
  rate$t_interval <- as.numeric(rate$t_interval)
  
  # Plot the mean shuttling rate
  plot <- ggplot2::ggplot(rate, ggplot2::aes_string(x = "t_interval", y = column)) +
    ggplot2::geom_line(color = "black") +
    ggplot2::geom_point(color = "#F79518") +
    ggplot2::labs(title = paste("Mean ", column,  "over the trial"),
                  x = "Time (h)",
                  y = paste("Mean ", column))+
    ggplot2::theme_light()
  
  print(rate)
  
  return(plot)
}
