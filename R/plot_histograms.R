#' Plot frequency distributions of key shuttle-box metrics
#'
#' This function plots frequency distributions for key shuttle-box metrics across individuals in the data se
#'
#' @param proj_data Project data containg shuttle-box metrics, as output by calc_project_data
#' @param bin_size_Tpref Bin size for temperature preference histogram, default = 1
#' @param bin_size_Tavoid_upper Bin size for upper avoidance temperature histogram, default  = 1
#' @param bin_size_Tavoid_lower Bin size for lower avoidance temperature histogram, default = 1 
#' @param bin_size_distance Bin size for distance histogram, default = 2000
#' @param bin_size_shuttles Bin size for number of shuttles, default = 20
#' @param bin_size_t_near_limits Bin size for time spent near limits, default = 5
#' @param nr_sd Number of standard deviations for outlier cutoff for normally distributed variables, default = 2
#' @param iqr_multiplier Interquartile range multiplier for outlier cutoff for exponentially distributed variable (time near limits), default = 1.5
#' @param id_col Column to be used for identifier labels, default = "fileID"
#' @import ggplot2 gridExtra
#' @return Plot of frequency distributions for key shuttle-box metrics across individuals in dataset
#' @export

plot_histograms <- function(proj_data, bin_size_Tpref = 1, bin_size_Tavoid_upper = 1, bin_size_Tavoid_lower = 1, bin_size_distance = 2000, bin_size_shuttles = 20, bin_size_t_near_limits = 5, nr_sd = 2, iqr_multiplier = 1.5, id_col = "fileID") {
  # Ensure necessary columns exist
  required_columns <- c("Tpref", "Tavoid_upper", "Tavoid_lower", "tot_distance", "nr_shuttles", "t_near_limits")
  if (!all(required_columns %in% colnames(proj_data))) {
    stop("The dataset does not contain one or more necessary columns: 'Tpref', 'Tavoid_upper', 'Tavoid_lower', 'tot_distance', 'nr_shuttles', 't_near_limits'.")
  }
  
  # Create individual plots
  p1 <- ggplot2::ggplot(proj_data, ggplot2::aes(x = Tpref)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count)),
                            binwidth = bin_size_Tpref, 
                            fill = "#F79518", 
                            color = "black") +
    ggplot2::stat_function(fun = function(x) dnorm(x, 
                                                   mean = mean(proj_data$Tpref, na.rm = TRUE), 
                                                   sd = sd(proj_data$Tpref, na.rm = TRUE)) * length(proj_data$Tpref) * bin_size_Tpref,
                           linewidth = 1, 
                           color = 'darkorange2') +
    ggplot2::labs(x = "Temperature preference (Celsius)", 
                  y = "Frequency") +
    ggplot2::theme_light()
  
  # p2: Distribution of Tavoid_upper
  p2 <- ggplot2::ggplot(proj_data, ggplot2::aes(x = Tavoid_upper)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count)),
                            binwidth = bin_size_Tavoid_upper, 
                            fill = "#E41A1C", 
                            color = "black") +
    ggplot2::stat_function(fun = function(x) dnorm(x, 
                                                   mean = mean(proj_data$Tavoid_upper, na.rm = TRUE), 
                                                   sd = sd(proj_data$Tavoid_upper, na.rm = TRUE)) * length(proj_data$Tavoid_upper) * bin_size_Tavoid_upper,
                           linewidth = 1, 
                           color = 'darkred') +  
    ggplot2::labs(x = "Upper avoidance temperature (Celsius)", y = "Frequency") +
    ggplot2::theme_light()
  
  # p3: Distribution of Tavoid_lower
  p3 <- ggplot2::ggplot(proj_data, ggplot2::aes(x = Tavoid_lower)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count)),
                            binwidth = bin_size_Tavoid_lower, 
                            fill = "#377EB8", 
                            color = "black") +
    ggplot2::stat_function(fun = function(x) dnorm(x, 
                                                   mean = mean(proj_data$Tavoid_lower, na.rm = TRUE), 
                                                   sd = sd(proj_data$Tavoid_lower, na.rm = TRUE)) * length(proj_data$Tavoid_lower) * bin_size_Tavoid_lower,
                           linewidth = 1, 
                           color = 'darkblue') +  
    ggplot2::labs(x = "Lower avoidance temperature (Celsius)", y = "Frequency") +
    ggplot2::theme_light()
  
  # p4: Distribution of Distance
  p4 <- ggplot2::ggplot(proj_data, ggplot2::aes(x = tot_distance)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count)),
                            binwidth = bin_size_distance, 
                            fill = "#4DAF4A", 
                            color = "black") +
    ggplot2::stat_function(fun = function(x) dnorm(x, 
                                                   mean = mean(proj_data$tot_distance, na.rm = TRUE), 
                                                   sd = sd(proj_data$tot_distance, na.rm = TRUE)) * length(proj_data$tot_distance) * bin_size_distance,
                           linewidth = 1, 
                           color = 'darkgreen') +  
    ggplot2::labs(x = "Distance", y = "Frequency") +
    ggplot2::theme_light()
  
  # p5: Distribution of Shuttles
  p5 <- ggplot2::ggplot(proj_data, ggplot2::aes(x = nr_shuttles)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count)),
                            binwidth = bin_size_shuttles, 
                            fill = "#984EA3", 
                            color = "black") +
    ggplot2::stat_function(fun = function(x) dnorm(x, 
                                                   mean = mean(proj_data$nr_shuttles, na.rm = TRUE), 
                                                   sd = sd(proj_data$nr_shuttles, na.rm = TRUE)) * length(proj_data$nr_shuttles) * bin_size_shuttles,
                           linewidth = 1, 
                           color = 'purple') +  
    ggplot2::labs(x = "Nr shuttles", y = "Frequency") +
    ggplot2::theme_light()
  
  # p6: Distribution of Time Near Limits
  p6 <- ggplot2::ggplot(proj_data, ggplot2::aes(x = t_near_limits)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count)),
                            binwidth = bin_size_t_near_limits, 
                            fill = "#EEDD82", 
                            color = "black") +
    ggplot2::stat_function(fun = function(x) dexp(x,
                                                  rate = 1 / mean(proj_data$t_near_limits, na.rm = TRUE)) *
                             length(proj_data$t_near_limits) * bin_size_t_near_limits,
                           linewidth = 1, 
                           color = 'orange') +  
    ggplot2::labs(x = "Time near limits", y = "Frequency") +
    ggplot2::theme_light()
  
  variables <- c("Tpref", "Tavoid_upper", "Tavoid_lower", "tot_distance", "nr_shuttles", "t_near_limits")
  
  # Function to detect outliers based on Z-score for selected variables and return a dataframe with outlier ids
  detect_outliers <- function(data, variables) {
    variables <- c("Tpref", "Tavoid_upper", "Tavoid_lower", "tot_distance", "nr_shuttles", "t_near_limits")
    outlier_df <- data.frame(matrix(NA, nrow = nrow(data), ncol = length(variables)))  # Initialize dataframe with NA
    # Set column names of the dataframe to the variable names
    colnames(outlier_df) <- variables
    
    # Loop through the specified variables
    for (var in variables) {
      
      if (var == "t_near_limits") {  # Use IQR for t_near_limits
        Q1 <- quantile(data[[var]], 0.25, na.rm = TRUE)
        Q3 <- quantile(data[[var]], 0.75, na.rm = TRUE)
        IQR_value <- Q3 - Q1
        upper_bound <- Q3 + iqr_multiplier * IQR_value  # Ignore lower bound for exponential data
        
        outlier_indices <- which(data[[var]] > upper_bound)
        
      } else {
        # Calculate the Z-scores for the variable
        z_scores <- (data[[var]] - mean(data[[var]], na.rm = TRUE)) / sd(data[[var]], na.rm = TRUE)
        # Identify outliers
        outlier_indices <- which(abs(z_scores) > nr_sd)
      }
      
      # Place outlier ids in the respective columns, keeping others as NA
      outlier_df[outlier_indices, var] <- data[[id_col]][outlier_indices]
      
    }
    
    return(outlier_df)
  }
  outlier_df<-detect_outliers(proj_data, variables)
  
  names_Tpref <- proj_data[!is.na(proj_data$Tpref) & proj_data[[id_col]] %in% outlier_df$Tpref, c(id_col, "Tpref")]
  colnames(names_Tpref) <- c("id","Tpref")
  plot_build <- ggplot2::ggplot_build(p1)
  y_min <- plot_build$layout$panel_params[[1]]$y.range[1]
  y_max <- plot_build$layout$panel_params[[1]]$y.range[2]
  y_mid <- (y_min + y_max) / 2  
  
  p1 <- p1 + 
    ggplot2::geom_vline(data = names_Tpref, ggplot2::aes(xintercept = Tpref), color = "blue", linetype = "dashed") +
    ggplot2::geom_text(data = names_Tpref, ggplot2::aes(x = Tpref, y = y_mid, label = id), angle = 90, vjust = -1, color = "blue", position = ggplot2::position_jitter(width = 0, height = 3))
  
  names_Tavoid_upper <- proj_data[!is.na(proj_data$Tavoid_upper) & proj_data[[id_col]] %in% outlier_df$Tavoid_upper, c(id_col, "Tavoid_upper")]
  colnames(names_Tavoid_upper) <- c("id","Tavoid_upper")
  plot_build <- ggplot2::ggplot_build(p2)
  y_min <- plot_build$layout$panel_params[[1]]$y.range[1]
  y_max <- plot_build$layout$panel_params[[1]]$y.range[2]
  y_mid <- (y_min + y_max) / 2
  
  p2 <- p2 + 
    ggplot2::geom_vline(data = names_Tavoid_upper, ggplot2::aes(xintercept = Tavoid_upper), color = "blue", linetype = "dashed") +
    ggplot2::geom_text(data = names_Tavoid_upper, ggplot2::aes(x = Tavoid_upper, y = y_mid, label = id), angle = 90, vjust = -1, color = "blue", position = ggplot2::position_jitter(width = 0, height = 3))
  
  names_Tavoid_lower <- proj_data[!is.na(proj_data$Tavoid_lower) & proj_data[[id_col]] %in% outlier_df$Tavoid_lower, c(id_col, "Tavoid_lower")]
  colnames(names_Tavoid_lower) <- c("id","Tavoid_lower")
  plot_build <- ggplot2::ggplot_build(p2)
  y_min <- plot_build$layout$panel_params[[1]]$y.range[1]
  y_max <- plot_build$layout$panel_params[[1]]$y.range[2]
  y_mid <- (y_min + y_max) / 2
  
  p3 <- p3 + 
    ggplot2::geom_vline(data = names_Tavoid_lower, ggplot2::aes(xintercept = Tavoid_lower), color = "blue", linetype = "dashed") +
    ggplot2::geom_text(data = names_Tavoid_lower, ggplot2::aes(x = Tavoid_lower, y = y_mid, label = id), angle = 90, vjust = -1, color = "blue", position = ggplot2::position_jitter(width = 0, height = 3))
  
  names_distance <- proj_data[!is.na(proj_data$tot_distance) & proj_data[[id_col]] %in% outlier_df$tot_distance, c(id_col, "tot_distance")]
  colnames(names_distance) <- c("id","tot_distance")
  plot_build <- ggplot2::ggplot_build(p2)
  y_min <- plot_build$layout$panel_params[[1]]$y.range[1]
  y_max <- plot_build$layout$panel_params[[1]]$y.range[2]
  y_mid <- (y_min + y_max) / 2
  
  p4 <- p4 + 
    ggplot2::geom_vline(data = names_distance, ggplot2::aes(xintercept = tot_distance), color = "blue", linetype = "dashed") +
    ggplot2::geom_text(data = names_distance, ggplot2::aes(x = tot_distance, y = y_mid, label = id), angle = 90, vjust = -1, color = "blue", position = ggplot2::position_jitter(width = 0, height = 3))
  
  names_nr_shuttles <- proj_data[!is.na(proj_data$nr_shuttles) & proj_data[[id_col]] %in% outlier_df$nr_shuttles, c(id_col, "nr_shuttles")]
  colnames(names_nr_shuttles) <- c("id","nr_shuttles")
  plot_build <- ggplot2::ggplot_build(p2)
  y_min <- plot_build$layout$panel_params[[1]]$y.range[1]
  y_max <- plot_build$layout$panel_params[[1]]$y.range[2]
  y_mid <- (y_min + y_max) / 2
  
  p5 <- p5 + 
    ggplot2::geom_vline(data = names_nr_shuttles, ggplot2::aes(xintercept = nr_shuttles), color = "blue", linetype = "dashed") +
    ggplot2::geom_text(data = names_nr_shuttles, ggplot2::aes(x = nr_shuttles, y = y_mid, label = id), angle = 90, vjust = -1, color = "blue", position = ggplot2::position_jitter(width = 0, height = 3))
  
  names_t_near_limits <- proj_data[!is.na(proj_data$t_near_limits) & proj_data[[id_col]] %in% outlier_df$t_near_limits, c(id_col, "t_near_limits")]
  colnames(names_t_near_limits) <- c("id","t_near_limits")
  plot_build <- ggplot2::ggplot_build(p2)
  y_min <- plot_build$layout$panel_params[[1]]$y.range[1]
  y_max <- plot_build$layout$panel_params[[1]]$y.range[2]
  y_mid <- (y_min + y_max) / 2
  
  p6 <- p6 + 
    ggplot2::geom_vline(data = names_t_near_limits, ggplot2::aes(xintercept = t_near_limits), color = "blue", linetype = "dashed") +
    ggplot2::geom_text(data = names_t_near_limits, ggplot2::aes(x = t_near_limits, y = y_mid, label = id), angle = 90, vjust = -1, color = "blue", position = ggplot2::position_jitter(width = 0, height = 3))
  
  # Combine the plots into a multipanel plot
  multipanel_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
  
  print(invisible(multipanel_plot))
  return(outlier_df)
}
