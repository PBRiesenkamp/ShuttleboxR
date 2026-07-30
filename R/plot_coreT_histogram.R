#' Plot the distribution of core body temperatures
#'
#' Plots the percentage of observations within fixed-width `core_T` bins. The
#' plot can show the midpoint of the most frequently occupied temperature bin
#' and the effective selected thermal breadth calculated from the same bins.
#'
#' @param data An organised shuttle-box data frame containing `core_T`.
#' @param bin_size Width of the temperature bins in degrees Celsius. Default is
#'   0.1. Use the same value when comparing animals.
#' @param exclude_start_minutes Minutes excluded from the beginning of the
#'   recording. Default is 0.
#' @param exclude_end_minutes Minutes excluded from the end of the recording.
#'   Default is 0.
#' @param exclude_acclimation Logical. Exclude rows labelled `"acclimation"`.
#'   Default is `FALSE`.
#' @param show_Tpref Logical. Show a dashed line at the midpoint of the most
#'   frequently occupied temperature bin. Default is `TRUE`.
#' @param show_Tbreadth Logical. Report effective selected thermal breadth in
#'   the plot subtitle. Default is `TRUE`.
#'
#' @return Invisibly returns the `ggplot` object, allowing it to be saved or
#'   further customised.
#'
#' @examples
#' example_file <- system.file(
#'   "extdata", "Fish_8_13_3_example.csv",
#'   package = "ShuttleboxR"
#' )
#' fish <- read_shuttlesoft(example_file)
#' plot_coreT_histogram(fish)
#'
#' @seealso [calc_Tbreadth()], [calc_Tpref()]
#' @import ggplot2
#' @export
plot_coreT_histogram <- function(data,
                                 bin_size = 0.1,
                                 exclude_start_minutes = 0,
                                 exclude_end_minutes = 0,
                                 exclude_acclimation = FALSE,
                                 show_Tpref = TRUE,
                                 show_Tbreadth = TRUE) {

  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }

  if (!"core_T" %in% names(data)) {
    stop("The dataset does not contain a `core_T` column.", call. = FALSE)
  }

  if (length(bin_size) != 1L || !is.numeric(bin_size) ||
      !is.finite(bin_size) || bin_size <= 0) {
    stop("`bin_size` must be one number greater than zero.", call. = FALSE)
  }

  if (!"time_sec" %in% names(data)) {
    data$time_sec <- seq.int(0L, nrow(data) - 1L)
  }

  time_values <- suppressWarnings(as.numeric(data$time_sec))
  start_time <- exclude_start_minutes * 60
  end_time <- max(time_values, na.rm = TRUE) - exclude_end_minutes * 60

  if (end_time < start_time) {
    stop("The requested start/end exclusions remove the entire recording.", call. = FALSE)
  }

  keep <- is.finite(time_values) & time_values >= start_time & time_values <= end_time
  data <- data[keep, , drop = FALSE]

  if (isTRUE(exclude_acclimation)) {
    if (!"trial_phase" %in% names(data)) {
      stop(
        "To exclude acclimation, supply `trial_start` when importing the file.",
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

  bin_id <- floor(temperatures / bin_size)
  counts <- table(bin_id)
  proportions <- as.numeric(counts) / sum(counts)

  peak_bin_id <- as.numeric(names(counts)[which.max(counts)])
  Tpref_peak <- (peak_bin_id + 0.5) * bin_size
  Tbreadth <- bin_size / sum(proportions^2)

  subtitle_parts <- character(0)
  if (isTRUE(show_Tpref)) {
    subtitle_parts <- c(
      subtitle_parts,
      paste0("Peak Tpref bin: ", round(Tpref_peak, 2), " °C")
    )
  }
  if (isTRUE(show_Tbreadth)) {
    subtitle_parts <- c(
      subtitle_parts,
      paste0("Effective thermal breadth: ", round(Tbreadth, 2), " °C")
    )
  }

  plot_data <- data.frame(core_T = temperatures)

  hist_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = core_T)) +
    ggplot2::geom_histogram(
      binwidth = bin_size,
      boundary = 0,
      closed = "left",
      ggplot2::aes(
        y = ggplot2::after_stat(count / sum(count) * 100)
      ),
      fill = "#F79518",
      colour = "black"
    ) +
    ggplot2::labs(
      title = "Distribution of core body temperatures",
      subtitle = if (length(subtitle_parts) > 0L) {
        paste(subtitle_parts, collapse = " | ")
      } else {
        NULL
      },
      x = "Core body temperature (°C)",
      y = "Percentage of observations (%)"
    ) +
    ggplot2::theme_light()

  if (isTRUE(show_Tpref)) {
    hist_plot <- hist_plot +
      ggplot2::geom_vline(
        xintercept = Tpref_peak,
        colour = "#2F855A",
        linetype = "dashed",
        linewidth = 1.2
      )
  }

  print(hist_plot)
  invisible(hist_plot)
}
