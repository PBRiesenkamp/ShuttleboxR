#' Calculate selected thermal breadth
#'
#' Measures how widely separated the temperatures experienced by a fish were.
#' Tbreadth is the mean absolute difference between the core temperatures at
#' two independently selected observations from the trial:
#'
#' `Tb = mean(abs(T_i - T_j))`
#'
#' This quantity is also known as the Gini mean difference. It uses the complete
#' distribution of `core_T`, including both the frequency of each temperature
#' and the distance between temperatures. It does not use histogram bins and is
#' not centred on `Tpref`.
#'
#' A fish that remains at nearly one temperature has a Tbreadth close to zero.
#' A fish that regularly experiences temperatures far apart has a larger
#' Tbreadth. For example, a fish spending half its time at 10 degrees Celsius
#' and half at 20 degrees Celsius has a Tbreadth of 5 degrees Celsius: half of
#' all pairs have the same temperature and half differ by 10 degrees Celsius.
#'
#' Tbreadth describes the overall spread of the distribution but cannot, by
#' itself, show whether the histogram is symmetrical, skewed, or multimodal.
#' Interpret it together with [plot_coreT_histogram()]. It is not a minimum-to-
#' maximum range, does not define lower and upper boundaries, and is not a
#' physiological thermal-tolerance limit.
#'
#' @param data An organised shuttle-box data frame containing `core_T`.
#' @param exclude_start_minutes Minutes to exclude from the beginning of the
#'   recording. Default is 0.
#' @param exclude_end_minutes Minutes to exclude from the end of the recording.
#'   Default is 0.
#' @param exclude_acclimation Logical. Exclude rows labelled `"acclimation"` in
#'   `trial_phase`. Default is `FALSE`.
#' @param exclude_gravitation Logical. Exclude the transitional gravitation
#'   period. Default is `FALSE` for backwards compatibility.
#' @param gravitation_time Optional advanced override: a gravitation duration
#'   in hours, usually from [calc_gravitation()]. Most users can leave this
#'   unset. When omitted and `exclude_gravitation = TRUE`, gravitation is
#'   estimated automatically using the same acclimation reference.
#' @param print_results Logical. Print the result. Default is `TRUE`.
#'
#' @return A single non-negative selected thermal breadth in degrees Celsius.
#'
#' @examples
#' \dontrun{
#' fish <- read_shuttlesoft(file.choose())
#' calc_Tbreadth(fish, exclude_gravitation = TRUE)
#' }
#'
#' @seealso [calc_Tpercentile_range()], [plot_coreT_histogram()], [calc_Tpref()], [calc_Tavoid()]
#' @export
calc_Tbreadth <- function(data,
                          exclude_start_minutes = 0,
                          exclude_end_minutes = 0,
                          exclude_acclimation = FALSE,
                          print_results = TRUE,
                          exclude_gravitation = FALSE,
                          gravitation_time = NULL) {

  data <- .prepare_trial_window(
    data,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation,
    exclude_gravitation = exclude_gravitation,
    gravitation_time = gravitation_time,
    context = "Tbreadth calculation"
  )

  if (!"core_T" %in% names(data)) {
    stop(
      "The dataset does not contain `core_T`. Import a ShuttleSoft file with `read_shuttlesoft()` or provide a `core_T` column.",
      call. = FALSE
    )
  }

  temperatures <- suppressWarnings(as.numeric(data$core_T))
  temperatures <- temperatures[is.finite(temperatures)]

  if (length(temperatures) == 0L) {
    stop("No valid `core_T` values remain after exclusions.", call. = FALSE)
  }

  thermal_breadth <- .temperature_gmd(temperatures)

  if (isTRUE(print_results)) {
    message(
      "Selected thermal breadth (mean pairwise difference): ",
      round(thermal_breadth, 3),
      " degrees Celsius"
    )
  }

  thermal_breadth
}

# Exact empirical Gini mean difference in O(n log n) time. This equals the
# average absolute difference between two independent draws from the observed
# temperature distribution, including pairs with the same observation value.
.temperature_gmd <- function(x) {
  x <- sort(as.numeric(x[is.finite(x)]))
  n <- length(x)

  if (n <= 1L) {
    return(0)
  }

  index <- seq_len(n)
  value <- 2 * sum((2 * index - n - 1) * x) / (n^2)

  # Avoid returning a tiny negative value caused by floating-point rounding.
  unname(max(0, value))
}
