#' Plot a correlation matrix of selected shuttle-box metrics
#'
#' This function plots the number of missing tracks per interval
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param columns Specify the columns to be used in plotting the correlation matrix
#' @param interval_minutes Specify the interval length in minutes, default is 10
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @import ggplot2 
#' @return Correlation matrix of selected shuttle-box metrics
#' @export

correlation_matrix <- function(data, columns, exclude_start_minutes = 0, exclude_end_minutes = 0, interval_minutes = 10) {
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec, na.rm = TRUE) - (exclude_end_minutes * 60)
  data_2 <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  # Create time intervals
  data_2$t_interval <- floor((data_2$time_sec - start_time) / (interval_minutes * 60)) * interval_minutes
  data
  # Compute mean values for selected columns within each interval
  mean_data <- aggregate(data_2[, columns, drop = FALSE], 
                           by = list(t_interval = data_2$t_interval), 
                           FUN = function(x) mean(x, na.rm = TRUE))
  
  # Function to display correlation values
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor = 1.2, ...) {
    r <- cor(x, y, use = "complete.obs")  # Compute correlation
    txt <- formatC(r, format = "f", digits = digits)  # Format value
    txt <- paste0(prefix, txt)  # Append prefix if needed
    
    # Place text at the center of the panel
    text(mean(range(x)), mean(range(y)), txt, cex = cex.cor)
  }
  
  # Generate correlation matrix plot
  pairs(
    mean_data[, columns, drop = FALSE],  # Use mean-aggregated data
    lower.panel = panel.smooth,  # Scatter plots with smoothing
    upper.panel = panel.cor  # Correlation values
  )
}
