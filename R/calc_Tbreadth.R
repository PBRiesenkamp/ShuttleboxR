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
#' @param print_results Logical. Print the result. Default is `TRUE`.
#'
#' @return A single non-negative selected thermal breadth in degrees Celsius.
#'
#' @examples
#' example_file <- system.file(
#'   "extdata", "Fish_7_13_2.csv",
#'   package = "ShuttleboxR"
#' )
#' fish <- read_shuttlesoft(example_file)
#' calc_Tbreadth(fish, print_results = FALSE)
#'
#' # Simple examples
#' calc_Tbreadth(data.frame(core_T = rep(20, 100)), print_results = FALSE)
#' calc_Tbreadth(
#'   data.frame(core_T = c(rep(10, 50), rep(20, 50))),
#'   print_results = FALSE
#' )
#'
#' @seealso [plot_coreT_histogram()], [calc_Tpref()], [calc_Tavoid()]
#' @export
calc_Tbreadth <- function(data,
                          exclude_start_minutes = 0,
                          exclude_end_minutes = 0,
                          exclude_acclimation = FALSE,
                          print_results = TRUE) {

  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }

  if (!"core_T" %in% names(data)) {
    stop(
      "The dataset does not contain `core_T`. Import a ShuttleSoft file with `read_shuttlesoft()` or provide a `core_T` column.",
      call. = FALSE
    )
  }

  numeric_scalar <- function(x, name) {
    if (length(x) != 1L || is.na(x) || !is.numeric(x) || !is.finite(x) || x < 0) {
      stop("`", name, "` must be one finite number that is zero or greater.", call. = FALSE)
    }
  }

  numeric_scalar(exclude_start_minutes, "exclude_start_minutes")
  numeric_scalar(exclude_end_minutes, "exclude_end_minutes")

  if (!"time_sec" %in% names(data)) {
    data$time_sec <- seq.int(0L, nrow(data) - 1L)
  }

  time_values <- suppressWarnings(as.numeric(data$time_sec))
  if (!any(is.finite(time_values))) {
    stop("`time_sec` contains no valid values.", call. = FALSE)
  }

  end_of_recording <- max(time_values, na.rm = TRUE)
  start_time <- exclude_start_minutes * 60
  end_time <- end_of_recording - exclude_end_minutes * 60

  if (end_time < start_time) {
    stop("The requested start/end exclusions remove the entire recording.", call. = FALSE)
  }

  keep <- is.finite(time_values) & time_values >= start_time & time_values <= end_time
  data <- data[keep, , drop = FALSE]

  if (isTRUE(exclude_acclimation)) {
    if (!"trial_phase" %in% names(data)) {
      stop(
        "To exclude acclimation, import with `read_shuttlesoft(..., trial_start = \"HH:MM:SS\")` or provide a `trial_phase` column.",
        call. = FALSE
      )
    }
    data <- data[data$trial_phase != "acclimation", , drop = FALSE]
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
