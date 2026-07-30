#' Plot distance versus shuttles across project data
#'
#' Plots total movement distance against the number of shuttles for each trial
#' or individual in a project-results table.
#'
#' @param proj_data Project-results data frame.
#' @param id_col Identifier column used for labels. Default is `"fileID"`.
#' @param label_points Logical. Label individual points. Default is `TRUE`.
#'
#' @return A `ggplot2` plot, invisibly.
#' @import ggplot2
#' @export
plot_distance_vs_shuttles <- function(proj_data,
                                      id_col = "fileID",
                                      label_points = TRUE) {
  proj_data <- .standardise_project_data(proj_data)
  required <- c(id_col, "tot_distance", "nr_shuttles")
  missing <- setdiff(required, names(proj_data))
  if (length(missing) > 0L) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  proj_data$.label_id <- as.character(proj_data[[id_col]])

  plot <- ggplot2::ggplot(
    proj_data,
    ggplot2::aes(x = tot_distance, y = nr_shuttles)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = "Distance moved and shuttling across individuals",
      x = "Total distance moved",
      y = "Number of shuttles"
    ) +
    ggplot2::theme_light()

  if (isTRUE(label_points)) {
    plot <- plot + ggplot2::geom_text(
      ggplot2::aes(label = .label_id),
      vjust = -0.7,
      check_overlap = TRUE
    )
  }

  print(plot)
  invisible(plot)
}
