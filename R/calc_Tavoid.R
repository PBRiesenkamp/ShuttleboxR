#' Calculate avoidance temperatures
#'
#' Calculates lower and upper percentile boundaries of the selected
#' core-temperature distribution.
#'
#' When `exclude_gravitation = TRUE`, observations before the gravitation
#' breakpoint are removed. The breakpoint is added to the start of the dynamic
#' period when `exclude_acclimation = TRUE`, or to the start of the complete
#' recording when `exclude_acclimation = FALSE`.
#'
#' @param data An organised shuttle-box data frame containing `core_T`.
#' @param percentiles Lower and upper percentiles. Default is `c(0.05, 0.95)`.
#' @param exclude_start_minutes Minutes omitted from the start of the selected
#'   period. Default is 0.
#' @param exclude_end_minutes Minutes omitted from the end of the recording.
#'   Default is 0.
#' @param exclude_acclimation Logical. Use only the dynamic period. Default is
#'   `FALSE`.
#' @param exclude_gravitation Logical. Exclude the transitional gravitation
#'   period. Default is `FALSE` for backwards compatibility.
#' @param gravitation_time Advanced optional override in hours. Most users can
#'   leave this unset; when `exclude_gravitation = TRUE`, gravitation is
#'   estimated automatically. When omitted and `exclude_gravitation = TRUE`, it is
#'   estimated automatically.
#' @param print_results Logical. Print the results. Default is `TRUE`.
#'
#' @return A two-element vector containing lower and upper avoidance
#'   temperatures in degrees Celsius.
#'
#' @examples
#' \dontrun{
#' fish <- read_shuttlesoft(file.choose())
#' calc_Tavoid(fish, exclude_gravitation = TRUE)
#' }
#'
#' @export
calc_Tavoid <- function(data,
                        percentiles = c(0.05, 0.95),
                        exclude_start_minutes = 0,
                        exclude_end_minutes = 0,
                        exclude_acclimation = FALSE,
                        print_results = TRUE,
                        exclude_gravitation = FALSE,
                        gravitation_time = NULL) {
  if (length(percentiles) != 2L || any(!is.finite(percentiles)) ||
      any(percentiles < 0) || any(percentiles > 1) ||
      percentiles[1L] >= percentiles[2L]) {
    stop("`percentiles` must contain two increasing values between 0 and 1.", call. = FALSE)
  }

  data <- .prepare_trial_window(
    data,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation,
    exclude_gravitation = exclude_gravitation,
    gravitation_time = gravitation_time,
    context = "avoidance-temperature calculation"
  )

  if (!"core_T" %in% names(data)) {
    stop("The dataset does not contain `core_T`.", call. = FALSE)
  }
  temperatures <- suppressWarnings(as.numeric(data$core_T))
  temperatures <- temperatures[is.finite(temperatures)]
  if (length(temperatures) == 0L) {
    stop("No valid `core_T` values remain after exclusions.", call. = FALSE)
  }

  values <- as.numeric(stats::quantile(
    temperatures,
    probs = percentiles,
    na.rm = TRUE,
    names = FALSE
  ))
  names(values) <- c("lower", "upper")

  if (isTRUE(print_results)) {
    message("Tavoid lower: ", round(values[1L], 3), " degrees Celsius")
    message("Tavoid upper: ", round(values[2L], 3), " degrees Celsius")
  }
  values
}
