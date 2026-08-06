#' Calculate the number of shuttles
#'
#' Counts chamber transitions within the selected analysis window.
#'
#' @param data An organised shuttle-box data frame containing `shuttle` and
#'   `time_sec`.
#' @param exclude_start_minutes Minutes omitted from the start of the selected
#'   period. Default is 0.
#' @param exclude_end_minutes Minutes omitted from the end of the recording.
#'   Default is 0.
#' @param exclude_acclimation Logical. Use only the dynamic period. Default is
#'   `FALSE`.
#' @param exclude_gravitation Logical. Exclude the transitional gravitation
#'   period. Default is `FALSE`; whole-trial activity is often of interest.
#' @param gravitation_time Advanced optional override in hours. Most users can
#'   leave this unset; when `exclude_gravitation = TRUE`, gravitation is
#'   estimated automatically.
#' @param print_results Logical. Print the result. Default is `TRUE`.
#'
#' @return Number of chamber transitions.
#' @examples
#' \dontrun{
#' fish <- read_shuttlesoft(file.choose())
#' calc_shuttles(fish)
#' calc_shuttles(fish, exclude_gravitation = TRUE)
#' }
#'
#' @export
calc_shuttles <- function(data,
                          exclude_start_minutes = 0,
                          exclude_end_minutes = 0,
                          exclude_acclimation = FALSE,
                          print_results = TRUE,
                          exclude_gravitation = FALSE,
                          gravitation_time = NULL) {
  if (!"shuttle" %in% names(data)) {
    stop("The dataset does not contain `shuttle`; run `file_prepare()`.", call. = FALSE)
  }
  data <- .prepare_trial_window(
    data,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation,
    exclude_gravitation = exclude_gravitation,
    gravitation_time = gravitation_time,
    context = "shuttle-count calculation"
  )
  shuttle_values <- suppressWarnings(as.numeric(data$shuttle))
  if (length(shuttle_values) > 0L) {
    shuttle_values[1L] <- 0
  }
  shuttles <- sum(shuttle_values, na.rm = TRUE)
  if (isTRUE(print_results)) {
    message("Number of shuttles: ", shuttles)
  }
  unname(shuttles)
}
