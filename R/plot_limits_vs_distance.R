#' Plot distance versus time spent near limits across project data
#'
#' Plots total movement distance against the percentage of observations spent
#' close to the programmed temperature limits. Optional review guides highlight
#' trials exceeding a transparent limit-exposure threshold and trials with
#' unusually low or high movement within the project. Highlighted cases require
#' inspection of the original trial and are not automatic exclusions.
#'
#' @param proj_data Project-results data frame.
#' @param id_col Identifier column used for labels. Default is `"fileID"`.
#' @param label_points Logical. Label points. When `highlight_cases = TRUE`,
#'   only highlighted cases are labelled. Default is `TRUE`.
#' @param highlight_cases Logical. Highlight potential review cases. Default is
#'   `FALSE`.
#' @param lower_quantile Lower movement quantile used for screening. Default is
#'   0.05.
#' @param upper_quantile Upper movement quantile used for screening. Default is
#'   0.95.
#' @param limits_iqr_multiplier Multiplier used when `limit_method = "iqr"` to
#'   define the cutoff as `Q3 + multiplier * IQR`. Default is 1.5.
#' @param limit_method Method used to define elevated limit exposure. One of
#'   `"absolute"` (default), `"quantile"`, or `"iqr"`. The absolute method is
#'   recommended for zero-heavy datasets because it retains a direct biological
#'   interpretation.
#' @param limit_threshold Percentage threshold used when
#'   `limit_method = "absolute"`. Default is 10, meaning more than 10 percent of
#'   analysed observations near either programmed limit.
#' @param limit_quantile Project quantile used when
#'   `limit_method = "quantile"`. Default is 0.95.
#' @param return_cases Logical. Return a list containing the plot, highlighted
#'   cases and cutoffs. Default is `FALSE`.
#'
#' @return A `ggplot2` plot invisibly, or a list with `plot`, `cases`, `cutoffs`,
#'   and `limit_method` when `return_cases = TRUE`.
#' @import ggplot2
#' @export
plot_limits_vs_distance <- function(proj_data,
                                    id_col = "fileID",
                                    label_points = TRUE,
                                    highlight_cases = FALSE,
                                    lower_quantile = 0.05,
                                    upper_quantile = 0.95,
                                    limits_iqr_multiplier = 1.5,
                                    return_cases = FALSE,
                                    limit_method = c("absolute", "quantile", "iqr"),
                                    limit_threshold = 10,
                                    limit_quantile = 0.95) {
  proj_data <- .standardise_project_data(proj_data)
  limit_method <- match.arg(limit_method)
  required <- c(id_col, "tot_distance", "t_near_limits")
  missing <- setdiff(required, names(proj_data))
  if (length(missing) > 0L) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  .validate_review_quantiles(lower_quantile, upper_quantile)

  plot_data <- proj_data[
    is.finite(proj_data$tot_distance) & is.finite(proj_data$t_near_limits),
    , drop = FALSE
  ]
  if (nrow(plot_data) == 0L) {
    stop("No complete finite observations are available for plotting.", call. = FALSE)
  }
  plot_data$.label_id <- as.character(plot_data[[id_col]])

  limits_high <- .calculate_limit_cutoff(
    plot_data$t_near_limits,
    method = limit_method,
    absolute_threshold = limit_threshold,
    quantile_probability = limit_quantile,
    iqr_multiplier = limits_iqr_multiplier
  )
  cutoffs <- c(
    limits_high = limits_high,
    distance_low = stats::quantile(plot_data$tot_distance, lower_quantile, na.rm = TRUE, names = FALSE),
    distance_high = stats::quantile(plot_data$tot_distance, upper_quantile, na.rm = TRUE, names = FALSE)
  )

  plot_data$.review_case <- .classify_limits_activity(
    limits = plot_data$t_near_limits,
    activity = plot_data$tot_distance,
    limits_high = cutoffs[["limits_high"]],
    activity_low = cutoffs[["distance_low"]],
    activity_high = cutoffs[["distance_high"]],
    activity_name = "movement"
  )
  cases <- plot_data[plot_data$.review_case != "Typical project range", c(
    id_col, "t_near_limits", "tot_distance", ".review_case"
  ), drop = FALSE]
  names(cases)[names(cases) == ".review_case"] <- "review_reason"

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = t_near_limits, y = tot_distance)
  ) +
    ggplot2::labs(
      title = "Time near temperature limits and distance moved",
      subtitle = if (isTRUE(highlight_cases)) {
        .limit_screen_text(
          limit_method,
          cutoffs[["limits_high"]],
          limit_quantile,
          activity_name = "movement"
        )
      } else {
        NULL
      },
      x = "Observations near temperature limits (%)",
      y = "Total distance moved (cm)"
    ) +
    ggplot2::theme_classic()

  if (isTRUE(highlight_cases)) {
    flagged <- plot_data[plot_data$.review_case != "Typical project range", , drop = FALSE]
    plot <- plot +
      ggplot2::geom_point(colour = "grey65") +
      ggplot2::geom_vline(
        xintercept = cutoffs[["limits_high"]],
        linetype = "dashed", colour = "grey55"
      ) +
      ggplot2::geom_hline(
        yintercept = cutoffs[c("distance_low", "distance_high")],
        linetype = "dashed", colour = "grey55"
      ) +
      ggplot2::geom_point(
        data = flagged,
        ggplot2::aes(colour = .review_case),
        size = 2.6
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
    if (isTRUE(label_points)) {
      plot <- plot + ggplot2::geom_text(
        ggplot2::aes(label = .label_id),
        vjust = -0.7,
        check_overlap = TRUE
      )
    }
  }

  if (isTRUE(return_cases)) {
    return(list(
      plot = plot,
      cases = cases,
      cutoffs = cutoffs,
      limit_method = limit_method
    ))
  }

  print(plot)
  invisible(plot)
}
