#' Calculate gravitation time
#'
#' Estimates the time taken for core body temperature to approach its settled
#' region using a segmented regression with one breakpoint.
#'
#' The returned value is a duration in hours measured from the selected origin:
#' the start of the dynamic period when `exclude_acclimation = TRUE`, or the
#' start of the complete recording when `exclude_acclimation = FALSE`.
#' `exclude_start_minutes` is also interpreted relative to that same origin.
#'
#' Downstream functions do not normally require this value to be passed
#' manually. Setting `exclude_gravitation = TRUE` in functions such as
#' [calc_Tpref()], [calc_Tbreadth()], [calc_Tpercentile_range()],
#' [calc_Tavoid()], and
#' [plot_coreT_histogram()] makes them call `calc_gravitation()` internally. The
#' returned value is mainly useful for inspection, reporting, or an advanced
#' manual override.
#'
#' @param data An organised shuttle-box data frame containing `time_sec` and
#'   `core_T`.
#' @param exclude_acclimation Logical. If `TRUE`, gravitation begins at the
#'   start of the dynamic period. If `FALSE`, it begins at the start of the
#'   complete recording. Default is `FALSE`.
#' @param exclude_start_minutes Minutes omitted from the beginning of the
#'   selected origin before fitting. The reported gravitation time remains
#'   measured from the selected origin. Default is 0.
#' @param exclude_end_minutes Minutes omitted from the end of the recording
#'   before fitting. Default is 0.
#' @param print_results Logical. Print the estimated duration. Default is
#'   `TRUE`.
#'
#' @return A single gravitation time in hours.
#'
#' @examples
#' \dontrun{
#' fish <- read_shuttlesoft(file.choose())
#' calc_gravitation(fish)
#' }
#'
#' @seealso [plot_T_segmented()], [calc_Tpref()], [calc_Tbreadth()]
#' @import segmented
#' @export
calc_gravitation <- function(data,
                             exclude_start_minutes = 0,
                             exclude_end_minutes = 0,
                             exclude_acclimation = FALSE,
                             print_results = TRUE) {
  gravitation_fit <- .fit_gravitation_model(
    data = data,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation
  )

  gravitation_time <- gravitation_fit$breakpoint_h

  if (isTRUE(print_results)) {
    reference <- if (isTRUE(exclude_acclimation)) {
      "dynamic-period start"
    } else {
      "recording start"
    }
    message(
      "Gravitation time: ", round(gravitation_time, 3),
      " hours from the ", reference
    )
  }

  gravitation_time
}
