#' Plot upper versus lower temperature-limit exposure
#'
#' Compares the percentage of observations spent near the lower and upper
#' programmed temperature limits. The plot can highlight fish with unusually
#' high exposure to the upper limit, lower limit, or both. This is a screening
#' step that should be followed by inspection of the original trial.
#'
#' @param proj_data Project-results data frame containing `t_near_min` and
#'   `t_near_max`.
#' @param id_col Identifier column used for labels. Default is `"fileID"`.
#' @param label_points Logical. Label highlighted points. Default is `TRUE`.
#' @param highlight_cases Logical. Highlight potential review cases using an IQR
#'   rule separately for the upper and lower limits. Default is `TRUE`.
#' @param iqr_multiplier Multiplier in `Q3 + multiplier * IQR`. Default is 1.5.
#' @param return_cases Logical. Return a list containing the plot, highlighted
#'   cases and cutoffs. Default is `FALSE`.
#'
#' @return A `ggplot2` plot invisibly, or a list with `plot`, `cases` and
#'   `cutoffs` when `return_cases = TRUE`.
#' @import ggplot2
#' @export
plot_upper_vs_lower_extremes <- function(proj_data,
                                         id_col = "fileID",
                                         label_points = TRUE,
                                         highlight_cases = TRUE,
                                         iqr_multiplier = 1.5,
                                         return_cases = FALSE) {
  proj_data <- .standardise_project_data(proj_data)
  required <- c(id_col, "t_near_min", "t_near_max")
  missing <- setdiff(required, names(proj_data))
  if (length(missing) > 0L) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  .validate_positive_scalar(iqr_multiplier, "iqr_multiplier", allow_zero = TRUE)

  plot_data <- proj_data[
    is.finite(proj_data$t_near_min) & is.finite(proj_data$t_near_max),
    , drop = FALSE
  ]
  plot_data$.label_id <- as.character(plot_data[[id_col]])

  lower_q1 <- stats::quantile(plot_data$t_near_min, 0.25, na.rm = TRUE, names = FALSE)
  lower_q3 <- stats::quantile(plot_data$t_near_min, 0.75, na.rm = TRUE, names = FALSE)
  upper_q1 <- stats::quantile(plot_data$t_near_max, 0.25, na.rm = TRUE, names = FALSE)
  upper_q3 <- stats::quantile(plot_data$t_near_max, 0.75, na.rm = TRUE, names = FALSE)

  cutoffs <- c(
    lower_limit_high = lower_q3 + iqr_multiplier * (lower_q3 - lower_q1),
    upper_limit_high = upper_q3 + iqr_multiplier * (upper_q3 - upper_q1)
  )

  high_lower <- plot_data$t_near_min > cutoffs[["lower_limit_high"]]
  high_upper <- plot_data$t_near_max > cutoffs[["upper_limit_high"]]
  plot_data$.review_case <- "Typical project range"
  plot_data$.review_case[high_lower] <- "High lower-limit exposure"
  plot_data$.review_case[high_upper] <- "High upper-limit exposure"
  plot_data$.review_case[high_lower & high_upper] <- "High exposure to both limits"

  cases <- plot_data[plot_data$.review_case != "Typical project range", c(
    id_col, "t_near_min", "t_near_max", ".review_case"
  ), drop = FALSE]
  names(cases)[names(cases) == ".review_case"] <- "review_reason"

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = t_near_min, y = t_near_max)
  ) +
    ggplot2::labs(
      title = "Exposure to lower and upper temperature limits",
      subtitle = if (isTRUE(highlight_cases)) {
        "Dashed lines show project-level IQR screening thresholds"
      } else {
        NULL
      },
      x = "Observations near lower limit (%)",
      y = "Observations near upper limit (%)"
    ) +
    ggplot2::theme_light()

  if (isTRUE(highlight_cases)) {
    flagged <- plot_data[plot_data$.review_case != "Typical project range", , drop = FALSE]
    plot <- plot +
      ggplot2::geom_point(colour = "grey65") +
      ggplot2::geom_vline(
        xintercept = cutoffs[["lower_limit_high"]],
        linetype = "dashed", colour = "grey55"
      ) +
      ggplot2::geom_hline(
        yintercept = cutoffs[["upper_limit_high"]],
        linetype = "dashed", colour = "grey55"
      ) +
      ggplot2::geom_point(
        data = flagged,
        ggplot2::aes(colour = .review_case),
        size = 2.8
      ) +
      ggplot2::labs(colour = "Potential review pattern")

    if (isTRUE(label_points) && nrow(flagged) > 0L) {
      plot <- plot + ggrepel::geom_text_repel(
        data = flagged,
        ggplot2::aes(label = .label_id, colour = .review_case),
        show.legend = FALSE,
        max.overlaps = Inf
      )
    }
  } else {
    plot <- plot + ggplot2::geom_point()
  }

  if (isTRUE(return_cases)) {
    return(list(plot = plot, cases = cases, cutoffs = cutoffs))
  }

  print(plot)
  invisible(plot)
}
