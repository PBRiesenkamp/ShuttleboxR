#' Plot a histogram of a chosen shuttle-box metric
#'
#' This function plots a histogram of the frequency of values observed for a chosen shuttle-box metric
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param column Select the name of the column with the variable the histogram needs to be plotted from
#' @param binwidth Binwidth of the histogram 
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @import ggplot2
#' @return A histogram of a chosen shuttle-box metric
#' @export

plot_histogram <- function(data, column, binwidth = 0.1, exclude_start_minutes = 0, exclude_end_minutes = 0) {
  # Ensure necessary column exists
  if (!paste(column) %in% colnames(data)) {
    stop(paste0("The dataset does not contain a '",column,"' column"))
  }
  
  # Convert time_sec to numeric if not already
  data$time_sec <- as.numeric(data$time_sec)
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec, na.rm = TRUE) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  # Create the histogram plot
  plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = column)) +
    ggplot2::geom_histogram(binwidth = binwidth, fill = "#F79518", color = "black") +
    ggplot2::labs(title = paste("Histogram of", column),
                  x = column,
                  y = "Frequency") +
    ggplot2::theme_light()
  
  print(plot)
}
