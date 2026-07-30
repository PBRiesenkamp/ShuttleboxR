#' Calculate effective selected thermal breadth
#'
#' Calculates an effective breadth from the frequency distribution of core body
#' temperatures. Temperatures are placed into equal-width bins, the proportion
#' of observations in each bin is calculated, and breadth is defined as:
#'
#' `bin_size / sum(p^2)`
#'
#' where `p` is the proportion of observations in each occupied bin. This is a
#' Simpson/Hill-number effective breadth expressed in degrees Celsius. It is
#' small when observations are concentrated around one temperature and larger
#' when time is distributed broadly and evenly across temperatures.
#'
#' This metric describes selected or experienced temperatures during the trial;
#' it is not a measure of physiological thermal tolerance.
#'
#' @param data An organised shuttle-box data frame containing `core_T`.
#' @param bin_size Temperature-bin width in degrees Celsius. The default is 0.1.
#'   Use the same value for all animals being compared.
#' @param exclude_start_minutes Minutes to exclude from the beginning of the
#'   recording. Default is 0.
#' @param exclude_end_minutes Minutes to exclude from the end of the recording.
#'   Default is 0.
#' @param exclude_acclimation Logical. Exclude rows labelled `"acclimation"` in
#'   `trial_phase`. Default is `FALSE`.
#' @param print_results Logical. Print the result. Default is `TRUE`.
#'
#' @return A single effective thermal breadth in degrees Celsius.
#'
#' @examples
#' example_file <- system.file(
#'   "extdata", "Fish_8_13_3_example.csv",
#'   package = "ShuttleboxR"
#' )
#' fish <- read_shuttlesoft(example_file)
#' calc_Tbreadth(fish, print_results = FALSE)
#'
#' @seealso [plot_coreT_histogram()], [calc_Tpref()], [calc_Tavoid()]
#' @export
calc_Tbreadth <- function(data,
                          bin_size = 0.1,
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

  numeric_scalar <- function(x, name, allow_zero = TRUE) {
    if (length(x) != 1L || is.na(x) || !is.numeric(x) || !is.finite(x)) {
      stop("`", name, "` must be one finite number.", call. = FALSE)
    }
    if ((allow_zero && x < 0) || (!allow_zero && x <= 0)) {
      comparison <- if (allow_zero) "zero or greater" else "greater than zero"
      stop("`", name, "` must be ", comparison, ".", call. = FALSE)
    }
  }

  numeric_scalar(bin_size, "bin_size", allow_zero = FALSE)
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

  # Anchoring bins to zero keeps bin boundaries consistent among animals.
  bin_id <- floor(temperatures / bin_size)
  counts <- as.numeric(table(bin_id))
  proportions <- counts / sum(counts)
  thermal_breadth <- bin_size / sum(proportions^2)
  thermal_breadth <- unname(thermal_breadth)

  if (isTRUE(print_results)) {
    message(
      "Effective selected thermal breadth: ",
      round(thermal_breadth, 3),
      " degrees Celsius"
    )
  }

  thermal_breadth
}
