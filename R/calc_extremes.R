#' Calculate exposure near programmed temperature limits
#'
#' Calculates the percentage of observations spent near the lower and upper
#' programmed temperature limits within the selected analysis window.
#'
#' @param data An organised shuttle-box data frame containing `time_sec`,
#'   `core_T`, `min_T`, and `max_T`.
#' @param threshold Width of each extreme-temperature zone in degrees Celsius.
#'   The default is 20 percent of the programmed temperature range.
#' @param exclude_start_minutes Minutes omitted from the start of the selected
#'   period. Default is 0.
#' @param exclude_end_minutes Minutes omitted from the end of the recording.
#'   Default is 0.
#' @param exclude_acclimation Logical. Use only the dynamic period. Default is
#'   `FALSE`.
#' @param exclude_gravitation Logical. Exclude the transitional gravitation
#'   period. Default is `FALSE`.
#' @param gravitation_time Advanced optional override in hours. Most users can
#'   leave this unset; when `exclude_gravitation = TRUE`, gravitation is
#'   estimated automatically.
#' @param print_results Logical. Print the results. Default is `TRUE`.
#'
#' @return A two-element vector giving percentages near the lower and upper
#'   limits.
#' @examples
#' \dontrun{
#' fish <- read_shuttlesoft(file.choose())
#' calc_extremes(fish, exclude_gravitation = TRUE)
#' }
#'
#' @export
calc_extremes <- function(
    data,
    threshold = 0.2 * (max(data$max_T, na.rm = TRUE) - max(data$min_T, na.rm = TRUE)),
    exclude_start_minutes = 0,
    exclude_end_minutes = 0,
    exclude_acclimation = FALSE,
    print_results = TRUE,
    exclude_gravitation = FALSE,
    gravitation_time = NULL) {

  required <- c("time_sec", "core_T", "min_T", "max_T")
  if (!all(required %in% names(data))) {
    stop("The dataset must contain `time_sec`, `core_T`, `min_T`, and `max_T`.", call. = FALSE)
  }
  .validate_nonnegative_number(threshold, "threshold")

  data <- .prepare_trial_window(
    data,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation,
    exclude_gravitation = exclude_gravitation,
    gravitation_time = gravitation_time,
    context = "limit-exposure calculation"
  )

  core_T <- suppressWarnings(as.numeric(data$core_T))
  upper_limit <- max(suppressWarnings(as.numeric(data$max_T)), na.rm = TRUE)
  lower_limit <- max(suppressWarnings(as.numeric(data$min_T)), na.rm = TRUE)
  valid <- is.finite(core_T)
  if (!any(valid) || !is.finite(upper_limit) || !is.finite(lower_limit)) {
    stop("No valid temperature-limit observations remain.", call. = FALSE)
  }

  upper_threshold <- upper_limit - threshold
  lower_threshold <- lower_limit + threshold
  lower <- mean(core_T[valid] < lower_threshold) * 100
  upper <- mean(core_T[valid] > upper_threshold) * 100
  values <- c(lower = lower, upper = upper)

  if (isTRUE(print_results)) {
    message("Time near lower limit: ", round(lower, 3), "%")
    message("Time near upper limit: ", round(upper, 3), "%")
  }
  values
}
