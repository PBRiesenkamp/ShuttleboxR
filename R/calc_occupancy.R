#' Calculate chamber occupancy
#'
#' Counts observations in the decreasing- and increasing-temperature chambers
#' within the selected analysis window.
#'
#' @param data An organised shuttle-box data frame containing `zone` and
#'   `time_sec`.
#' @param exclude_start_minutes Minutes omitted from the start of the selected
#'   period. Default is 0.
#' @param exclude_end_minutes Minutes omitted from the end of the recording.
#'   Default is 0.
#' @param exclude_acclimation Logical. Use only the dynamic period. Default is
#'   `FALSE`.
#' @param exclude_gravitation Logical. Exclude the transitional gravitation
#'   period. Default is `FALSE`.
#' @param gravitation_time Optional gravitation duration in hours, usually from
#'   [calc_gravitation()].
#' @param print_results Logical. Print the results. Default is `TRUE`.
#'
#' @return A two-element vector for DECR and INCR occupancy.
#' @export
calc_occupancy <- function(data,
                           exclude_start_minutes = 0,
                           exclude_end_minutes = 0,
                           exclude_acclimation = FALSE,
                           print_results = TRUE,
                           exclude_gravitation = FALSE,
                           gravitation_time = NULL) {
  if (!"zone" %in% names(data)) {
    stop("The dataset does not contain `zone`.", call. = FALSE)
  }
  data <- .prepare_trial_window(
    data,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation,
    exclude_gravitation = exclude_gravitation,
    gravitation_time = gravitation_time,
    context = "occupancy calculation"
  )
  values <- c(
    DECR = sum(as.character(data$zone) == "DECR", na.rm = TRUE),
    INCR = sum(as.character(data$zone) == "INCR", na.rm = TRUE)
  )
  if (isTRUE(print_results)) {
    message("Time in DECR chamber: ", values[1L], " observations")
    message("Time in INCR chamber: ", values[2L], " observations")
  }
  values
}
