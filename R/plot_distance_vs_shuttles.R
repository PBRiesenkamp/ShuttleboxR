#' Plot distance versus shuttles across project data
#'
#' Plots total movement distance against the number of shuttles for each trial
#' or individual. Optional review guides highlight fish at the margins of the
#' project distribution and describe the kind of pattern that deserves closer
#' inspection. These are screening categories, not automatic exclusions.
#'
#' @param proj_data Project-results data frame.
#' @param id_col Identifier column used for labels. Default is `"fileID"`.
#' @param label_points Logical. Label points. When `highlight_cases = TRUE`,
#'   only highlighted cases are labelled. Default is `TRUE`.
#' @param highlight_cases Logical. Highlight potential review cases using the
#'   lower and upper project quantiles. Default is `FALSE`.
#' @param lower_quantile Lower quantile used for screening. Default is 0.05.
#' @param upper_quantile Upper quantile used for screening. Default is 0.95.
#' @param return_cases Logical. Return a list containing the plot, highlighted
#'   cases and cutoffs. Default is `FALSE`, which invisibly returns the plot.
#'
#' @return A `ggplot2` plot invisibly, or a list with `plot`, `cases` and
#'   `cutoffs` when `return_cases = TRUE`.
#' @import ggplot2
#' @export
plot_distance_vs_shuttles <- function(proj_data,
                                      id_col = "fileID",
                                      label_points = TRUE,
                                      highlight_cases = FALSE,
                                      lower_quantile = 0.05,
                                      upper_quantile = 0.95,
                                      return_cases = FALSE) {
  proj_data <- .standardise_project_data(proj_data)
  required <- c(id_col, "tot_distance", "nr_shuttles")
  missing <- setdiff(required, names(proj_data))
  if (length(missing) > 0L) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  .validate_review_quantiles(lower_quantile, upper_quantile)

  plot_data <- proj_data[
    is.finite(proj_data$tot_distance) & is.finite(proj_data$nr_shuttles),
    , drop = FALSE
  ]
  plot_data$.label_id <- as.character(plot_data[[id_col]])

  cutoffs <- c(
    distance_low = stats::quantile(plot_data$tot_distance, lower_quantile, na.rm = TRUE, names = FALSE),
    distance_high = stats::quantile(plot_data$tot_distance, upper_quantile, na.rm = TRUE, names = FALSE),
    shuttles_low = stats::quantile(plot_data$nr_shuttles, lower_quantile, na.rm = TRUE, names = FALSE),
    shuttles_high = stats::quantile(plot_data$nr_shuttles, upper_quantile, na.rm = TRUE, names = FALSE)
  )

  plot_data$.review_case <- .classify_distance_shuttles(plot_data, cutoffs)
  cases <- plot_data[plot_data$.review_case != "Typical project range", c(
    id_col, "tot_distance", "nr_shuttles", ".review_case"
  ), drop = FALSE]
  names(cases)[names(cases) == ".review_case"] <- "review_reason"

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = tot_distance, y = nr_shuttles)
  ) +
    ggplot2::labs(
      title = "Distance moved and shuttling across individuals",
      subtitle = if (isTRUE(highlight_cases)) {
        "Highlighted points cross the selected project-level screening quantiles"
      } else {
        NULL
      },
      x = "Total distance moved (cm)",
      y = "Number of shuttles"
    ) +
    ggplot2::theme_light()

  if (isTRUE(highlight_cases)) {
    plot <- plot +
      ggplot2::geom_point(colour = "grey65") +
      ggplot2::geom_vline(
        xintercept = cutoffs[c("distance_low", "distance_high")],
        linetype = "dashed", colour = "grey55"
      ) +
      ggplot2::geom_hline(
        yintercept = cutoffs[c("shuttles_low", "shuttles_high")],
        linetype = "dashed", colour = "grey55"
      ) +
      ggplot2::geom_point(
        data = plot_data[plot_data$.review_case != "Typical project range", , drop = FALSE],
        ggplot2::aes(colour = .review_case),
        size = 2.6
      ) +
      ggplot2::labs(colour = "Potential review pattern")

    if (isTRUE(label_points) && nrow(cases) > 0L) {
      plot <- plot + ggrepel::geom_text_repel(
        data = plot_data[plot_data$.review_case != "Typical project range", , drop = FALSE],
        ggplot2::aes(label = .label_id, colour = .review_case),
        show.legend = FALSE,
        max.overlaps = Inf
      )
    }
  } else {
    plot <- plot + ggplot2::geom_point()
    if (isTRUE(label_points)) {
      plot <- plot + ggplot2::geom_text(
        ggplot2::aes(label = .label_id),
        vjust = -0.7,
        check_overlap = TRUE
      )
    }
  }

  if (isTRUE(return_cases)) {
    return(list(plot = plot, cases = cases, cutoffs = cutoffs))
  }

  print(plot)
  invisible(plot)
}
