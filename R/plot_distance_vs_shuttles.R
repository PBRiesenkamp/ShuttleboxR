#' Plot distance versus shuttles across project data
#'
#' This function plots the distance covered during the trial against the number of shuttles, across individual trials in the project data
#'
#' @param proj_data Project data containg shuttle-box metrics, as output by calc_project_data
#' @import ggplot2 
#' @return Plot of distance versus shuttles across project data
#' @export


plot_distance_vs_shuttles <- function(proj_data) {
  # Ensure necessary columns exist
  if (!all(c("fileID", "tot_distance", "nr_shuttles") %in% colnames(proj_data))) {
    stop("The dataset does not contain one or more necessary columns: 'fileID', 'tot_distance', 'nr_shuttles'.")
  }
  
  # Create the plot
  plot <- ggplot2::ggplot(proj_data, ggplot2::aes(x = tot_distance, y = nr_shuttles, label = fileID)) +
    ggplot2::geom_point(color = "#F79518") +
    ggplot2::geom_text(vjust = -1, hjust = 1.5) +
    ggplot2::labs(title = "Relationship between Distance Moved and Shuttles across Individuals",
                  x = "Distance Moved (cm)",
                  y = "Number of Shuttles") +
    ggplot2::theme_light()
  
  print(plot)
}
