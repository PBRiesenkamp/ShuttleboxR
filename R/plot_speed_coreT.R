#' Plot relation between velocity and core body temperature
#'
#' This function plots the velocity of the subject against the core body temperature
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @import ggplot2 
#' @return Plot of velocity versus core body temperature
#' @export

plot_speed_coreT <- function(data, exclude_start_minutes = 0, exclude_end_minutes = 0) {
  # Ensure necessary columns exist
  if (!all(c("core_T", "distance", "time_sec") %in% colnames(data))) {
    stop("The dataset does not contain one or more necessary columns: 'core_T', 'distance', 'time_sec'. Please run file_prepare function.")
  }
  
  # Convert time_sec to numeric if not already
  data$time_sec <- as.numeric(data$time_sec)
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  # Calculate movement speed as the difference in distance moved over time
  data$Movement_Speed <- c(NA, diff(data$distance) / diff(data$time_sec))
  
  # Remove rows with NA values
  data <- na.omit(data[, c("core_T", "Movement_Speed")])
  
  # Create the scatter plot
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = core_T, y = Movement_Speed)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_smooth(method = "loess", formula = 'y ~ x', color = "#F79518", se = FALSE) +
    ggplot2::labs(title = "Movement Speed vs. Core Body Temperature",
                  x = "Core Body Temperature (Celsius)",
                  y = "Movement Speed (cm/s)") +
    ggplot2::theme_light()
  
  return(plot)
}
