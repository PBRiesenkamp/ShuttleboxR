#' Calculate total distance moved
#'
#' Calculates distance accumulated within the selected analysis window.
#'
#' @param data An organised shuttle-box data frame containing cumulative
#'   `distance` and `time_sec`.
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
#' @return Total distance moved within the selected window.
#' @examples
#' \dontrun{
#' fish <- read_shuttlesoft(file.choose())
#' calc_tot_distance(fish)
#' calc_tot_distance(fish, exclude_gravitation = TRUE)
#' }
#'
#' @export
calc_tot_distance <- function(data,
                              exclude_start_minutes = 0,
                              exclude_end_minutes = 0,
                              exclude_acclimation = FALSE,
                              print_results = TRUE,
                              exclude_gravitation = FALSE,
                              gravitation_time = NULL) {
  if (!"distance" %in% names(data)) {
    stop("The dataset does not contain `distance`.", call. = FALSE)
  }
  data <- .prepare_trial_window(
    data,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation,
    exclude_gravitation = exclude_gravitation,
    gravitation_time = gravitation_time,
    context = "distance calculation"
  )
  distance <- suppressWarnings(as.numeric(data$distance))
  distance <- distance[is.finite(distance)]
  if (length(distance) == 0L) {
    stop("No valid distance values remain after exclusions.", call. = FALSE)
  }
  total_distance <- max(distance) - min(distance)
  if (isTRUE(print_results)) {
    message("Distance moved: ", round(total_distance, 3), " cm")
  }
  unname(total_distance)
}
