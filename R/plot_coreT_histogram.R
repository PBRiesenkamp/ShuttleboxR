#' Plot a histogram of core body temperatures
#'
#' This function plots a histogram of different core body temperatures observed during the trial
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param bin_size Bin size of the histogram, default is 0.1
#' @param exclude_acclimation Exclude the acclimation period from variable calculation, default is TRUE
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @import ggplot2
#' @return Histogram of the core body temperatures during the trial
#' @export

plot_coreT_histogram <- function(data, bin_size = 0.1, exclude_start_minutes = 0, exclude_end_minutes = 0, exclude_acclimation = F) {
  # Ensure the body core temperature column exists
  if (!"core_T" %in% colnames(data)) {
    stop("The dataset does not contain a 'core_T' column. Please run file_prepare and calc_coreT")
  }
  
  # Convert time_sec to numeric if not already
  data$time_sec <- as.numeric(data$time_sec)
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  
  if (exclude_acclimation) {
    data <- data[data$trial_phase != "acclimation", ]
  }
  
  
  # Calculate the duration of each bin in minutes
  data$Time_min <- data$time_sec / 60
  total_time <- max(data$Time_min, na.rm = TRUE)
  
  # Create the histogram data
  hist_data <- hist(data$core_T, breaks = seq(floor(min(data$core_T, na.rm = TRUE)),
                                              ceiling(max(data$core_T, na.rm = TRUE)),
                                              by = bin_size), plot = FALSE)
  
  # Find the peak value
  Tpref_peak <- hist_data$mids[which.max(hist_data$counts)]
  
  # Create the histogram plot
  hist_plot <- ggplot2::ggplot(data, ggplot2::aes(x = core_T)) +
    ggplot2::geom_histogram(binwidth = bin_size, ggplot2::aes(y = (ggplot2::after_stat(count) / sum(ggplot2::after_stat(count))) * 100), fill = "#F79518", color = "black") +
    ggplot2::geom_vline(xintercept = Tpref_peak, color = "#9EF1A4", linetype = "dashed", linewidth = 2) +
    ggplot2::labs(title = "Histogram of Percent of Time Spent at Different Body Core Temperatures",
                  subtitle = paste("Peak Tpref:", round(Tpref_peak, 2), "Celcius"),
                  x = "Body Core Temperature (Celsius)",
                  y = "Percent of Total Time (%)") +
    ggplot2::theme_light()
  
  print(hist_plot)
  print(paste("Peak Tpref:", Tpref_peak))
  # return(Tpref_peak)
}
