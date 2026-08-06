#' Plot temperature trajectory and gravitation breakpoint
#'
#' Displays core body temperature through time, the one-breakpoint segmented
#' fit used to estimate gravitation time, and the selected Tpref and avoidance
#' temperatures. The complete selected time window remains visible even when
#' `exclude_gravitation = TRUE`; only the horizontal thermal metrics are then
#' calculated from post-gravitation observations.
#'
#' The time axis begins at the start of the dynamic period when
#' `exclude_acclimation = TRUE`, or at the start of the complete recording when
#' `exclude_acclimation = FALSE`. The shaded region shows the gravitation period
#' measured from that origin.
#'
#' @param data An organised shuttle-box data frame containing `time_sec`,
#'   `core_T`, `INCR_T`, and `DECR_T`.
#' @param Tpref_method Method used by [calc_Tpref()]. Default is `"median"`.
#' @param Tavoid_percentiles Percentiles used by [calc_Tavoid()]. Default is
#'   `c(0.05, 0.95)`.
#' @param exclude_start_minutes Minutes omitted from the start of the selected
#'   period. Default is 0.
#' @param exclude_end_minutes Minutes omitted from the end of the recording.
#'   Default is 0.
#' @param exclude_acclimation Logical. Plot and fit from the dynamic-period
#'   start rather than the complete recording. Default is `TRUE`.
#' @param exclude_gravitation Logical. Calculate Tpref and avoidance
#'   temperatures using only observations after the breakpoint. Default is
#'   `TRUE`.
#' @param gravitation_time Optional advanced override: a gravitation duration
#'   in hours. Most users can leave this unset; the function estimates
#'   gravitation automatically. Supply a value only when a manually checked or
#'   adjusted breakpoint must be used.
#' @param overlay_chamber_temp Logical. Overlay warm- and cold-chamber
#'   temperatures. Default is `TRUE`.
#'
#' @return A `ggplot` object. The estimated gravitation time is stored in the
#'   `gravitation_time` attribute.
#'
#' @examples
#' \dontrun{
#' fish <- read_shuttlesoft(file.choose())
#' plot_T_segmented(fish)
#' }
#'
#' @import ggplot2 segmented
#' @export
plot_T_segmented <- function(data,
                             Tpref_method = "median",
                             Tavoid_percentiles = c(0.05, 0.95),
                             exclude_start_minutes = 0,
                             exclude_end_minutes = 0,
                             exclude_acclimation = TRUE,
                             overlay_chamber_temp = TRUE,
                             exclude_gravitation = TRUE,
                             gravitation_time = NULL) {
  required <- c("time_sec", "core_T")
  if (!all(required %in% names(data))) {
    stop("The dataset must contain `time_sec` and `core_T`.", call. = FALSE)
  }

  fit_result <- NULL
  if (is.null(gravitation_time)) {
    fit_result <- .fit_gravitation_model(
      data,
      exclude_start_minutes = exclude_start_minutes,
      exclude_end_minutes = exclude_end_minutes,
      exclude_acclimation = exclude_acclimation
    )
    gravitation_time <- fit_result$breakpoint_h
  } else {
    .validate_nonnegative_number(gravitation_time, "gravitation_time")
    fit_result <- tryCatch(
      .fit_gravitation_model(
        data,
        exclude_start_minutes = exclude_start_minutes,
        exclude_end_minutes = exclude_end_minutes,
        exclude_acclimation = exclude_acclimation
      ),
      error = function(e) {
        warning(
          "The supplied gravitation time will be shown, but the segmented line could not be fitted: ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
  }

  plot_data <- .prepare_trial_window(
    data,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation,
    exclude_gravitation = FALSE,
    context = "segmented temperature plot"
  )

  origin_sec <- .trial_origin_seconds(data, exclude_acclimation)
  plot_data$plot_time_h <- (as.numeric(plot_data$time_sec) - origin_sec) / 3600
  cutoff_h <- as.numeric(gravitation_time)

  # Use the fitted model for the orange segmented line. If a manually supplied
  # time differs from the model estimate, the vertical cutoff still reflects
  # the user's supplied value while the line shows the fitted trajectory.
  prediction_data <- NULL
  if (!is.null(fit_result)) {
    prediction_data <- fit_result$fit_data
    prediction_data$predicted <- as.numeric(stats::predict(fit_result$segmented_fit))
  }

  Tpref <- calc_Tpref(
    data,
    method = Tpref_method,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation,
    exclude_gravitation = exclude_gravitation,
    gravitation_time = gravitation_time,
    print_results = FALSE
  )
  Tavoid <- calc_Tavoid(
    data,
    percentiles = Tavoid_percentiles,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation,
    exclude_gravitation = exclude_gravitation,
    gravitation_time = gravitation_time,
    print_results = FALSE
  )

  x_min <- min(plot_data$plot_time_h, na.rm = TRUE)
  x_max <- max(plot_data$plot_time_h, na.rm = TRUE)
  y_min <- min(as.numeric(plot_data$core_T), na.rm = TRUE)
  y_max <- max(as.numeric(plot_data$core_T), na.rm = TRUE)
  y_span <- y_max - y_min
  if (!is.finite(y_span) || y_span == 0) y_span <- 1
  cutoff_display <- min(max(cutoff_h, x_min), x_max)

  subtitle <- if (isTRUE(exclude_gravitation)) {
    "Horizontal metrics use post-gravitation observations; the full selected trajectory is shown"
  } else {
    "Horizontal metrics use the full selected trajectory"
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = plot_time_h, y = as.numeric(core_T))
  ) +
    ggplot2::annotate(
      "rect",
      xmin = x_min,
      xmax = cutoff_display,
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.12,
      fill = "grey50"
    ) +
    ggplot2::geom_point(alpha = 0.55, size = 0.45) +
    ggplot2::geom_vline(
      xintercept = cutoff_h,
      linetype = "dotted",
      linewidth = 1
    ) +
    ggplot2::geom_hline(
      yintercept = Tpref,
      linetype = "dashed",
      colour = "#2F855A",
      linewidth = 1
    ) +
    ggplot2::geom_hline(
      yintercept = Tavoid[1L],
      linetype = "dashed",
      colour = "#3182BD",
      linewidth = 0.9
    ) +
    ggplot2::geom_hline(
      yintercept = Tavoid[2L],
      linetype = "dashed",
      colour = "#C53030",
      linewidth = 0.9
    ) +
    ggplot2::annotate(
      "text",
      x = x_max,
      y = Tpref + 0.015 * y_span,
      label = paste0("Tpref: ", round(Tpref, 2), " °C"),
      hjust = 1,
      vjust = 0,
      colour = "#2F855A",
      size = 3.4
    ) +
    ggplot2::annotate(
      "text",
      x = x_max,
      y = Tavoid[1L] + 0.015 * y_span,
      label = paste0("Lower Tavoid: ", round(Tavoid[1L], 2), " °C"),
      hjust = 1,
      vjust = 0,
      colour = "#3182BD",
      size = 3.4
    ) +
    ggplot2::annotate(
      "text",
      x = x_max,
      y = Tavoid[2L] + 0.015 * y_span,
      label = paste0("Upper Tavoid: ", round(Tavoid[2L], 2), " °C"),
      hjust = 1,
      vjust = 0,
      colour = "#C53030",
      size = 3.4
    ) +
    ggplot2::annotate(
      "text",
      x = cutoff_display,
      y = y_min + 0.04 * y_span,
      label = paste0("Gravitation: ", round(cutoff_h, 2), " h"),
      hjust = if (cutoff_h < (x_min + x_max) / 2) 0 else 1,
      vjust = 0,
      size = 3.7
    ) +
    ggplot2::labs(
      title = "Core body temperature and gravitation breakpoint",
      subtitle = subtitle,
      x = if (isTRUE(exclude_acclimation)) {
        "Time since dynamic-period start (h)"
      } else {
        "Time since recording start (h)"
      },
      y = "Core body temperature (°C)"
    ) +
    ggplot2::theme_classic()

  if (!is.null(prediction_data)) {
    p <- p + ggplot2::geom_line(
      data = prediction_data,
      ggplot2::aes(x = time_from_origin_h, y = predicted),
      inherit.aes = FALSE,
      colour = "#F79518",
      linewidth = 1
    )
  }

  if (isTRUE(overlay_chamber_temp) &&
      all(c("DECR_T", "INCR_T") %in% names(plot_data))) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = as.numeric(DECR_T)),
        colour = "dodgerblue",
        linewidth = 0.45,
        alpha = 0.75
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = as.numeric(INCR_T)),
        colour = "brown1",
        linewidth = 0.45,
        alpha = 0.75
      )
  }

  attr(p, "gravitation_time") <- gravitation_time
  p
}
