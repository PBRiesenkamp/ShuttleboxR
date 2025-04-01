#' Plot number of lost tracks per interval
#'
#' This function plots the number of missing tracks per interval
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param exclude_acclimation Exclude the acclimation period from variable calculation, default is TRUE
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @import ggplot2 
#' @return Plot of the number of missed tracks per interval
#' @export


plot_heatmap <- function(data, exclude_start_minutes = 0, exclude_end_minutes = 0, exclude_acclimation = F) {
  # Ensure necessary columns exist
  if (!all(c("x_pos", "y_pos", "time_sec") %in% colnames(data))) {
    stop("The dataset does not contain one or more necessary columns: 'x_pos', 'y_pos', 'time_sec'.")
  }
  
  # Convert time_sec to numeric if not already
  data$time_sec <- as.numeric(data$time_sec)
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec, na.rm = TRUE) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  if (exclude_acclimation) {
    data <- data[data$trial_phase != "acclimation", ]
  }
  
  # Get min and max x_pos for each temperature side
  cold_min <- min(data$x_pos[data$zone == "DECR"], na.rm = TRUE)
  cold_max <- max(data$x_pos[data$zone == "DECR"], na.rm = TRUE)
  warm_min <- min(data$x_pos[data$zone == "INCR"], na.rm = TRUE)
  warm_max <- max(data$x_pos[data$zone == "INCR"], na.rm = TRUE)
  
  # Determine the border between either zone
  split_position <- mean(c(min(cold_max, warm_max), max(cold_min, warm_min)))
  
  # Plot heatmap
  heatmap_plot <- ggplot2::ggplot(data, ggplot2::aes(x = x_pos, y = y_pos)) +
    ggplot2::stat_density2d(ggplot2::aes(fill = ggplot2::after_stat(density)), geom = "raster", contour = FALSE) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::geom_vline(xintercept = split_position, color = "white", linetype = "dashed", size = 1) +
    ggplot2::annotate("text", x = cold_min + (cold_max-cold_min)/4, y = min(data$y_pos, na.rm = TRUE)+20, 
                      label = "Cold Chamber", color = "dodgerblue", size = 5, hjust = 0) +
    ggplot2::annotate("text", x = warm_min + (warm_max-warm_min)/4, y = min(data$y_pos, na.rm = TRUE)+20, 
                      label = "Warm Chamber", color = "brown1", size = 5, hjust = 0) +
    ggplot2::labs(title = "Heat Map of Fish Locations within the Shuttlebox",
                  x = "X Position",
                  y = "Y Position") +
    ggplot2::theme_light() +
    ggplot2::coord_fixed()
  
  return(heatmap_plot)
}
