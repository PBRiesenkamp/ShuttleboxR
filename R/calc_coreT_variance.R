#' Calculate variation in core body temperature
#'
#' Calculates the standard error, standard deviation, or coefficient of
#' variation of `core_T` within the selected analysis window.
#'
#' @param data An organised shuttle-box data frame containing `core_T`.
#' @param variance_type One of `"std_error"`, `"std_deviation"`, or
#'   `"coeff_variation"`.
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
#'
#' @return A single measure of variation in core body temperature.
#' @examples
#' \dontrun{
#' fish <- read_shuttlesoft(file.choose())
#' calc_coreT_variance(fish, variance_type = "std_deviation",
#'   exclude_gravitation = TRUE)
#' }
#'
#' @export
calc_coreT_variance <- function(
    data,
    variance_type = c("std_error", "std_deviation", "coeff_variation"),
    exclude_start_minutes = 0,
    exclude_end_minutes = 0,
    exclude_acclimation = FALSE,
    exclude_gravitation = FALSE,
    gravitation_time = NULL) {

  variance_type <- match.arg(variance_type)
  data <- .prepare_trial_window(
    data,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation,
    exclude_gravitation = exclude_gravitation,
    gravitation_time = gravitation_time,
    context = "core-temperature variation calculation"
  )

  if (!"core_T" %in% names(data)) {
    stop("The dataset does not contain `core_T`.", call. = FALSE)
  }
  x <- suppressWarnings(as.numeric(data$core_T))
  x <- x[is.finite(x)]
  if (length(x) < 2L) {
    stop("At least two valid `core_T` observations are required.", call. = FALSE)
  }

  switch(
    variance_type,
    std_error = stats::sd(x) / sqrt(length(x)),
    std_deviation = stats::sd(x),
    coeff_variation = stats::sd(x) / mean(x)
  )
}
