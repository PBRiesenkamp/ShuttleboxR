#' Calculate tracking accuracy
#'
#' Calculates the proportion of observations with a valid x coordinate within
#' the selected analysis window.
#'
#' @param data An organised shuttle-box data frame containing `x_pos`.
#' @param exclude_start_minutes Minutes omitted from the start of the selected
#'   period. Default is 0.
#' @param exclude_end_minutes Minutes omitted from the end of the recording.
#'   Default is 0.
#' @param exclude_acclimation Logical. Use only the dynamic period. Default is
#'   `FALSE`.
#' @param print_results Logical. Print the result. Default is `TRUE`.
#'
#' @return Proportion of observations successfully tracked, from 0 to 1.
#' @export
calc_track_accuracy <- function(data,
                                exclude_start_minutes = 0,
                                exclude_end_minutes = 0,
                                exclude_acclimation = FALSE,
                                print_results = TRUE) {
  if (!"x_pos" %in% names(data)) {
    stop("The dataset does not contain `x_pos`.", call. = FALSE)
  }
  data <- .prepare_trial_window(
    data,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation,
    exclude_gravitation = FALSE,
    context = "tracking-accuracy calculation"
  )
  proportion <- mean(!is.na(data$x_pos))
  if (isTRUE(print_results)) {
    message("Proportion successfully tracked: ", round(proportion * 100, 3), "%")
  }
  unname(proportion)
}
