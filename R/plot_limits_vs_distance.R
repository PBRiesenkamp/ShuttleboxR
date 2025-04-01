#' Plot distance versus time spent near limits across project data
#'
#' This function plots the distance covered during the trial against the time spent near the set temperature limits, across individual trials in the project data
#'
#' @param proj_data Project data containg shuttle-box metrics, as output by calc_project_data
#' @import ggplot2 
#' @return Plot of distance versus time near limits across project data
#' @export

plot_limits_vs_distance <- function(proj_data) {
  # Ensure necessary columns exist
  if (!all(c("fileID", "tot_distance", "t_near_limits") %in% colnames(proj_data))) {
    stop("The dataset does not contain one or more necessary columns: 'fileID', 'tot_distance', 't_near_limits'.")
  }
  
  # Create the plot
  plot <- ggplot2::ggplot(proj_data, ggplot2::aes(x = t_near_limits, y = tot_distance, label = fileID)) +
    ggplot2::geom_point(color = "#F79518") +
    ggplot2::geom_text(vjust = -1, hjust = 1.5) +
    ggplot2::labs(title = "Relationship between Time Near Limits and Distance Moved",
                  x = "Time Near Limits (minutes)",
                  y = "Distance Moved (cm)") +
    ggplot2::theme_light()
  
  print(plot)
}
