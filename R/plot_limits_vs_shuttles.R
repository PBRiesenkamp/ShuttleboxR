#' Plot time spent near the limits versus shuttles across project data
#'
#' This function plots the the time spent near the set temperature limits against the number of shuttles, across individual trials in the project data
#'
#' @param proj_data Project data containg shuttle-box metrics, as output by calc_project_data
#' @import ggplot2 
#' @return Plot of limits versus shuttles across project data
#' @export

plot_limits_vs_shuttles <- function(proj_data) {
  # Ensure necessary columns exist
  if (!all(c("fileID", "t_near_limits", "nr_shuttles") %in% colnames(proj_data))) {
    stop("The dataset does not contain one or more necessary columns: 'fileID', t_near_limits', 'nr_shuttles'.")
  }
  
  # Create the plot
  plot <- ggplot2::ggplot(proj_data, ggplot2::aes(x = t_near_limits, y = nr_shuttles, label = fileID)) +
    ggplot2::geom_point(color = "#F79518") +
    ggplot2::geom_text(vjust = -1, hjust = 1.5) +
    ggplot2::labs(title = "Relationship between Time Near Limits and Number of shuttles",
                  x = "Time Near Limits (minutes)",
                  y = "Shuttles") +
    ggplot2::theme_light()
  
  print(plot)
}