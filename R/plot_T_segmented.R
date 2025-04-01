#' Plot the core body temperature during the trial
#'
#' This function plots the core body temperature during the trial, along with temperature preference, avoidance, and gravitation time.
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param Tpref_method The method used for calculation of temperature preference ("median", "mean", "mode"). Default is "median".
#' @param Tavoid_percentiles The lower and upper percentile for lower and upper avoidance temperature calculation resp. Default is c(0.05, 0.95)
#' @param exclude_acclimation Exclude the acclimation period from variable calculation, default is TRUE
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @param overlay_chamber_temp Boolean to determine whether or not to overlay chamber temperatures, default = TRUE
#' @import ggplot2 segmented
#' @return plot with the core body temperature and key shuttle-box metrics
#' @export

plot_T_segmented <- function(data, Tpref_method = "median", Tavoid_percentiles = c(0.05, 0.95),exclude_start_minutes = 0, exclude_end_minutes = 0, exclude_acclimation = TRUE, overlay_chamber_temp = T) {
  # Ensure necessary columns exist
  if (!all(c("time_h", "core_T") %in% colnames(data))) {
    stop("The dataset does not contain one or more necessary columns: 'time_h', 'core_T'.")
  }
  
  # Convert the time to numeric if not already
  data$time_sec <- as.numeric(data$time_sec)
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  if (exclude_acclimation) {
    fit <- lm(core_T ~ time_h, data = data[data$trial_phase != "acclimation", ])
    seg_fit <- segmented::segmented(fit, seg.Z = ~time_h, npsi = 1)
    predicted_values <- data.frame(
      time_h = data$time_h[data$trial_phase != "acclimation"],
      predicted = predict(seg_fit))
  } else {
    fit <- lm(core_T ~ time_h, data = data)
    seg_fit <- segmented::segmented(fit, seg.Z = ~time_h, npsi = 1)
    predicted_values <- data.frame(
      time_h = data$time_h,
      predicted = predict(seg_fit))
  }
  
  # Perform segmented regression
  acclimation_time <- min(data$time_h[data$trial_phase != "acclimation"], na.rm = TRUE)
  
  Tpref <- calc_Tpref(data, method = Tpref_method, print_results = F, exclude_start_minutes = exclude_start_minutes, exclude_end_minutes = exclude_end_minutes,  exclude_acclimation = exclude_acclimation)
  Tavoid <- calc_Tavoid(data, percentiles = Tavoid_percentiles, print_results = F, exclude_start_minutes = exclude_start_minutes, exclude_end_minutes = exclude_end_minutes,  exclude_acclimation = exclude_acclimation)
  Tavoid_lower <- Tavoid[1]
  Tavoid_upper <- Tavoid[2]
  
  # Get summary and breakpoint
  seg_summary <- summary(seg_fit)
  breakpoints <- seg_fit$psi[, "Est."]- acclimation_time
  
  # Plot data with segmented regression
  plot <-
    ggplot2::ggplot(data, ggplot2::aes(x = time_h, y = core_T)) +
    ggplot2::geom_point(alpha = 1, size = 0.4) +
    ggplot2::geom_line(data = predicted_values, ggplot2::aes(x = time_h, y = predicted), color = "#F79518", size = 1)+
    ggplot2::annotate("text", x = min(predicted_values$time_h), hjust = 0, y = min(data$core_T), label = paste("Gravitation time:", round(breakpoints, 1), "h"), color = "#F79518", size = 4.5)+
    ggplot2::geom_hline(yintercept = Tpref, linetype = "dashed", color = "#03DB6A", size = 1.75) +
    ggplot2::annotate("text", x = min(data$time_h), hjust = 0, y = Tpref + 0.4, label = paste("Preference:", round(Tpref, 1)), color = "#03DB6A", size = 4.5)+
    ggplot2::geom_hline(yintercept = Tavoid_lower, linetype = "dashed", color = "#9ECFF1", size = 1.75) +
    ggplot2::annotate("text", x = min(data$time_h) + exclude_start_minutes/60, hjust = 0, y = Tavoid_lower + 0.4, label = paste("Lower avoidance:", round(Tavoid_lower, 1)), color = "#9ECFF1", size = 4.5)+
    ggplot2::geom_hline(yintercept = Tavoid_upper, linetype = "dashed", color = "#F1AD9E", size = 1.75) +
    ggplot2::annotate("text", x = min(data$time_h) + exclude_start_minutes/60, hjust = 0, y = Tavoid_upper + 0.4, label = paste("Upper avoidance:", round(Tavoid_upper, 1)), color = "#F1AD9E", size = 4.5)+
    ggplot2::labs(title = "Body Core Temperature vs Time with Segmented Regression",
                  x = "Time (h)",
                  y = "Body Core Temperature (Celsius)")+
    ggplot2::theme_light()
  
  if(overlay_chamber_temp){
    plot <- plot + 
      ggplot2::geom_line(ggplot2::aes(y = `DECR_T`, color = "DECR side temp")) +
      ggplot2::geom_line(ggplot2::aes(y = `INCR_T`, color = "INCR side temp")) +
      ggplot2::scale_color_manual(values = c("DECR side temp" = "dodgerblue", "INCR side temp" = "brown1"), guide="none")
  }
  
  print(list(summary = seg_summary, breakpoints = breakpoints))
  
  return(plot)
  
}
