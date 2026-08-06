#' Plot the distribution of core body temperatures
#'
#' Plots the percentage of observations within fixed-width `core_T` bins. The
#' histogram shows the shape of the selected-temperature distribution, while
#' the subtitle can report median `Tpref` and Tbreadth. Tbreadth is calculated
#' from the original observations as the mean pairwise temperature difference;
#' the visual `bin_size` does not affect its value.
#'
#' @param data An organised shuttle-box data frame containing `core_T`.
#' @param bin_size Width of the displayed temperature bins in degrees Celsius.
#'   Default is 0.1. This affects only the appearance of the histogram.
#' @param exclude_start_minutes Minutes excluded from the beginning of the
#'   recording. Default is 0.
#' @param exclude_end_minutes Minutes excluded from the end of the recording.
#'   Default is 0.
#' @param exclude_acclimation Logical. Exclude rows labelled `"acclimation"`.
#'   Default is `FALSE`.
#' @param exclude_gravitation Logical. Exclude the transitional gravitation
#'   period before drawing the histogram. Default is `FALSE`.
#' @param gravitation_time Advanced optional override in hours. Most users can
#'   leave this unset; when `exclude_gravitation = TRUE`, gravitation is
#'   estimated automatically.
#' @param show_Tpref Logical. Show a dashed line at median `core_T`, the default
#'   definition of `Tpref`. Default is `TRUE`.
#' @param show_Tbreadth Logical. Report selected thermal breadth in the plot
#'   subtitle. Default is `TRUE`.
#'
#' @return Invisibly returns the `ggplot` object, allowing it to be saved or
#'   further customised.
#'
#' @examples
#' \dontrun{
#' fish <- read_shuttlesoft(file.choose())
#' plot_coreT_histogram(fish, exclude_gravitation = TRUE)
#' }
#'
#' @seealso [calc_Tbreadth()], [calc_Tpercentile_range()], [calc_Tpref()]
#' @import ggplot2
#' @export
plot_coreT_histogram <- function(data,
                                 bin_size = 0.1,
                                 exclude_start_minutes = 0,
                                 exclude_end_minutes = 0,
                                 exclude_acclimation = FALSE,
                                 show_Tpref = TRUE,
                                 show_Tbreadth = TRUE,
                                 exclude_gravitation = FALSE,
                                 gravitation_time = NULL) {

  if (length(bin_size) != 1L || !is.numeric(bin_size) ||
      !is.finite(bin_size) || bin_size <= 0) {
    stop("`bin_size` must be one number greater than zero.", call. = FALSE)
  }

  data <- .prepare_trial_window(
    data,
    exclude_start_minutes = exclude_start_minutes,
    exclude_end_minutes = exclude_end_minutes,
    exclude_acclimation = exclude_acclimation,
    exclude_gravitation = exclude_gravitation,
    gravitation_time = gravitation_time,
    context = "core-temperature histogram"
  )

  if (!"core_T" %in% names(data)) {
    stop("The dataset does not contain a `core_T` column.", call. = FALSE)
  }
  temperatures <- suppressWarnings(as.numeric(data$core_T))
  temperatures <- temperatures[is.finite(temperatures)]
  if (length(temperatures) == 0L) {
    stop("No valid `core_T` values remain after exclusions.", call. = FALSE)
  }

  Tpref_value <- stats::median(temperatures)
  Tbreadth_value <- .temperature_gmd(temperatures)

  subtitle_parts <- character(0)
  if (isTRUE(exclude_gravitation)) {
    subtitle_parts <- c(subtitle_parts, "Post-gravitation observations only")
  }
  if (isTRUE(show_Tpref)) {
    subtitle_parts <- c(
      subtitle_parts,
      paste0("Median Tpref: ", round(Tpref_value, 2), " °C")
    )
  }
  if (isTRUE(show_Tbreadth)) {
    subtitle_parts <- c(
      subtitle_parts,
      paste0(
        "Tbreadth (mean pairwise difference): ",
        round(Tbreadth_value, 2),
        " °C"
      )
    )
  }

  plot_data <- data.frame(core_T = temperatures)

  hist_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = core_T)) +
    ggplot2::geom_histogram(
      binwidth = bin_size,
      boundary = 0,
      closed = "left",
      ggplot2::aes(y = ggplot2::after_stat(count / sum(count) * 100)),
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
    ggplot2::theme_classic()

  if (isTRUE(show_Tpref)) {
    hist_plot <- hist_plot +
      ggplot2::geom_vline(
        xintercept = Tpref_value,
        colour = "#2F855A",
        linetype = "dashed",
        linewidth = 1.2
      )
  }

  print(hist_plot)
  invisible(hist_plot)
}
