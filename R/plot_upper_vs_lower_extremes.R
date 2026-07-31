#' Plot upper versus lower temperature-limit exposure
#'
#' Compares the percentage of observations spent near the lower and upper
#' programmed temperature limits. The plot can highlight fish exceeding a
#' transparent threshold at the upper limit, lower limit, or both. This is a
#' screening step that should be followed by inspection of the original trial.
#'
#' @param proj_data Project-results data frame containing `t_near_min` and
#'   `t_near_max`.
#' @param id_col Identifier column used for labels. Default is `"fileID"`.
#' @param label_points Logical. Label highlighted points. Default is `TRUE`.
#' @param highlight_cases Logical. Highlight potential review cases. Default is
#'   `TRUE`.
#' @param iqr_multiplier Multiplier used when `limit_method = "iqr"` to define
#'   each cutoff as `Q3 + multiplier * IQR`. Default is 1.5.
#' @param limit_method Method used to define elevated exposure at each limit.
#'   One of `"absolute"` (default), `"quantile"`, or `"iqr"`.
#' @param lower_limit_threshold Percentage threshold for lower-limit exposure
#'   when `limit_method = "absolute"`. Default is 10.
#' @param upper_limit_threshold Percentage threshold for upper-limit exposure
#'   when `limit_method = "absolute"`. Default is 10.
#' @param limit_quantile Project quantile used separately for lower- and
#'   upper-limit exposure when `limit_method = "quantile"`. Default is 0.95.
#' @param return_cases Logical. Return a list containing the plot, highlighted
#'   cases and cutoffs. Default is `FALSE`.
#'
#' @return A `ggplot2` plot invisibly, or a list with `plot`, `cases`, `cutoffs`,
#'   and `limit_method` when `return_cases = TRUE`.
#' @import ggplot2
#' @export
plot_upper_vs_lower_extremes <- function(proj_data,
                                         id_col = "fileID",
                                         label_points = TRUE,
                                         highlight_cases = TRUE,
                                         iqr_multiplier = 1.5,
                                         return_cases = FALSE,
                                         limit_method = c("absolute", "quantile", "iqr"),
                                         lower_limit_threshold = 10,
                                         upper_limit_threshold = 10,
                                         limit_quantile = 0.95) {
  proj_data <- .standardise_project_data(proj_data)
  limit_method <- match.arg(limit_method)
  required <- c(id_col, "t_near_min", "t_near_max")
  missing <- setdiff(required, names(proj_data))
  if (length(missing) > 0L) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  plot_data <- proj_data[
    is.finite(proj_data$t_near_min) & is.finite(proj_data$t_near_max),
    , drop = FALSE
  ]
  if (nrow(plot_data) == 0L) {
    stop("No complete finite observations are available for plotting.", call. = FALSE)
  }
  plot_data$.label_id <- as.character(plot_data[[id_col]])

  lower_cutoff <- .calculate_limit_cutoff(
    plot_data$t_near_min,
    method = limit_method,
    absolute_threshold = lower_limit_threshold,
    quantile_probability = limit_quantile,
    iqr_multiplier = iqr_multiplier
  )
  upper_cutoff <- .calculate_limit_cutoff(
    plot_data$t_near_max,
    method = limit_method,
    absolute_threshold = upper_limit_threshold,
    quantile_probability = limit_quantile,
    iqr_multiplier = iqr_multiplier
  )
  cutoffs <- c(
    lower_limit_high = lower_cutoff,
    upper_limit_high = upper_cutoff
  )

  high_lower <- plot_data$t_near_min > cutoffs[["lower_limit_high"]]
  high_upper <- plot_data$t_near_max > cutoffs[["upper_limit_high"]]
  plot_data$.review_case <- "Typical project range"
  plot_data$.review_case[high_lower] <- "Above lower-limit threshold"
  plot_data$.review_case[high_upper] <- "Above upper-limit threshold"
  plot_data$.review_case[high_lower & high_upper] <- "Above thresholds at both limits"

  cases <- plot_data[plot_data$.review_case != "Typical project range", c(
    id_col, "t_near_min", "t_near_max", ".review_case"
  ), drop = FALSE]
  names(cases)[names(cases) == ".review_case"] <- "review_reason"

  subtitle <- if (isTRUE(highlight_cases)) {
    if (limit_method == "absolute") {
      paste0(
        "Dashed lines show absolute exposure thresholds: lower > ",
        format(cutoffs[["lower_limit_high"]], trim = TRUE),
        "%; upper > ",
        format(cutoffs[["upper_limit_high"]], trim = TRUE),
        "%"
      )
    } else if (limit_method == "quantile") {
      paste0(
        "Dashed lines show separate project ",
        format(100 * limit_quantile, trim = TRUE),
        "th-percentile thresholds"
      )
    } else {
      "Dashed lines show separate project-level IQR thresholds"
    }
  } else {
    NULL
  }

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = t_near_min, y = t_near_max)
  ) +
    ggplot2::labs(
      title = "Exposure to lower and upper temperature limits",
      subtitle = subtitle,
      x = "Observations near lower limit (%)",
      y = "Observations near upper limit (%)"
    ) +
    ggplot2::theme_classic()

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
