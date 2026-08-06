#' Calculate temperature preference
#'
#' Calculates the centre of the selected core-temperature distribution using
#' the median, mean, or mode.
#'
#' When `exclude_gravitation = TRUE`, observations before the gravitation
#' breakpoint are removed. The breakpoint is added to the start of the dynamic
#' period when `exclude_acclimation = TRUE`, or to the start of the complete
#' recording when `exclude_acclimation = FALSE`.
#'
#' @param data An organised shuttle-box data frame containing `core_T`.
#' @param method Calculation method: `"median"`, `"mean"`, or `"mode"`.
#'   Default is `"median"`.
#' @param exclude_acclimation Logical. Use only the dynamic period. Default is
#'   `FALSE`.
#' @param exclude_start_minutes Minutes omitted from the start of the selected
#'   period. Default is 0.
#' @param exclude_end_minutes Minutes omitted from the end of the recording.
#'   Default is 0.
#' @param exclude_gravitation Logical. Exclude the transitional gravitation
#'   period. Default is `FALSE` for backwards compatibility.
#' @param gravitation_time Optional advanced override: a gravitation duration
#'   in hours, usually from [calc_gravitation()]. Most users can leave this
#'   unset. When omitted and `exclude_gravitation = TRUE`, gravitation is
#'   estimated automatically using the same acclimation reference.
#' @param print_results Logical. Print the result. Default is `TRUE`.
#'
#' @return A single temperature preference in degrees Celsius.
#'
#' @examples
#' \dontrun{
#' fish <- read_shuttlesoft(file.choose())
#' calc_Tpref(fish, exclude_gravitation = TRUE)
#' }
#'
#' @export
calc_Tpref <- function(data,
                       method = c("median", "mean", "mode"),
                       exclude_acclimation = FALSE,
                       exclude_start_minutes = 0,
                       exclude_end_minutes = 0,
                       print_results = TRUE,
                       exclude_gravitation = FALSE,
                       gravitation_time = NULL) {
  method <- match.arg(method)

  data <- .prepare_trial_window(
    data,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation,
    exclude_gravitation = exclude_gravitation,
    gravitation_time = gravitation_time,
    context = "Tpref calculation"
  )

  if (!"core_T" %in% names(data)) {
    stop("The dataset does not contain `core_T`.", call. = FALSE)
  }
  temperatures <- suppressWarnings(as.numeric(data$core_T))
  temperatures <- temperatures[is.finite(temperatures)]
  if (length(temperatures) == 0L) {
    stop("No valid `core_T` values remain after exclusions.", call. = FALSE)
  }

  Tpref <- switch(
    method,
    median = stats::median(temperatures, na.rm = TRUE),
    mean = mean(temperatures, na.rm = TRUE),
    mode = {
      counts <- sort(table(temperatures), decreasing = TRUE)
      as.numeric(names(counts)[1L])
    }
  )

  if (isTRUE(print_results)) {
    message("Tpref: ", round(Tpref, 3), " degrees Celsius")
  }
  unname(Tpref)
}
