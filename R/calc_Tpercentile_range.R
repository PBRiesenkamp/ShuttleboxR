#' Calculate a percentile-based thermal range
#'
#' Calculates the difference between two percentiles of the selected
#' core-temperature distribution. With the default 25th and 75th percentiles,
#' this is the interquartile range (IQR): the width of the central 50 percent of
#' experienced temperatures.
#'
#' This metric complements [calc_Tbreadth()]. `Tpercentile_range` describes the
#' width between two explicit percentile boundaries, whereas `Tbreadth` uses
#' the complete distribution and averages all pairwise temperature
#' differences. It also differs from `Tpref_range`, which is calculated from
#' the avoidance percentiles selected in [calc_Tavoid()].
#'
#' When `exclude_gravitation = TRUE`, gravitation is estimated automatically
#' unless `gravitation_time` is supplied as an advanced override.
#'
#' @param data An organised shuttle-box data frame containing `core_T`.
#' @param percentiles Lower and upper percentiles. Default is `c(0.25, 0.75)`.
#' @param exclude_start_minutes Minutes omitted from the start of the selected
#'   period. Default is 0.
#' @param exclude_end_minutes Minutes omitted from the end of the recording.
#'   Default is 0.
#' @param exclude_acclimation Logical. Use only the dynamic period. Default is
#'   `FALSE`.
#' @param print_results Logical. Print the percentile boundaries and their
#'   difference. Default is `TRUE`.
#' @param exclude_gravitation Logical. Exclude the transitional gravitation
#'   period. Default is `FALSE` for backwards compatibility.
#' @param gravitation_time Advanced optional override in hours. Most users can
#'   leave this unset; when `exclude_gravitation = TRUE`, gravitation is
#'   estimated automatically.
#'
#' @return A single non-negative percentile range in degrees Celsius.
#'
#' @examples
#' \dontrun{
#' fish <- read_shuttlesoft(file.choose())
#' calc_Tpercentile_range(fish, exclude_gravitation = TRUE)
#' calc_Tpercentile_range(
#'   fish,
#'   percentiles = c(0.10, 0.90),
#'   exclude_gravitation = TRUE
#' )
#' }
#'
#' @seealso [calc_Tbreadth()], [calc_Tavoid()], [plot_coreT_histogram()]
#' @export
calc_Tpercentile_range <- function(data,
                                   percentiles = c(0.25, 0.75),
                                   exclude_start_minutes = 0,
                                   exclude_end_minutes = 0,
                                   exclude_acclimation = FALSE,
                                   print_results = TRUE,
                                   exclude_gravitation = FALSE,
                                   gravitation_time = NULL) {
  if (length(percentiles) != 2L || any(!is.finite(percentiles)) ||
      any(percentiles < 0) || any(percentiles > 1) ||
      percentiles[1L] >= percentiles[2L]) {
    stop(
      "`percentiles` must contain two increasing values between 0 and 1.",
      call. = FALSE
    )
  }

  data <- .prepare_trial_window(
    data,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation,
    exclude_gravitation = exclude_gravitation,
    gravitation_time = gravitation_time,
    context = "thermal percentile-range calculation"
  )

  if (!"core_T" %in% names(data)) {
    stop("The dataset does not contain `core_T`.", call. = FALSE)
  }

  temperatures <- suppressWarnings(as.numeric(data$core_T))
  temperatures <- temperatures[is.finite(temperatures)]
  if (length(temperatures) == 0L) {
    stop("No valid `core_T` values remain after exclusions.", call. = FALSE)
  }

  bounds <- as.numeric(stats::quantile(
    temperatures,
    probs = percentiles,
    na.rm = TRUE,
    names = FALSE
  ))
  percentile_range <- unname(bounds[2L] - bounds[1L])

  if (isTRUE(print_results)) {
    labels <- paste0(round(percentiles * 100, 3), "%")
    message(
      "Thermal percentile range (", labels[1L], " to ", labels[2L], "): ",
      round(percentile_range, 3), " degrees Celsius",
      " [", round(bounds[1L], 3), " to ", round(bounds[2L], 3), "]"
    )
  }

  percentile_range
}
