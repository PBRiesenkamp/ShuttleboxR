#' Plot a correlation matrix of selected shuttle-box metrics
#'
#' Creates a pairs plot and returns the corresponding correlation matrix. The
#' function can be used with a project-results table containing one row per
#' trial, or with a single time-series trial that is first averaged into time
#' intervals.
#'
#' @param data A project-results data frame or an organised single-trial data
#'   frame.
#' @param columns Character vector naming the numeric columns to include.
#' @param exclude_start_minutes For single-trial data, minutes removed from the
#'   start. Ignored for project-results data.
#' @param exclude_end_minutes For single-trial data, minutes removed from the
#'   end. Ignored for project-results data.
#' @param interval_minutes For single-trial data, interval length used before
#'   calculating correlations. Default is 10 minutes.
#'
#' @return The numeric correlation matrix, invisibly.
#'
#' @examples
#' example_file <- system.file(
#'   "extdata", "project_database_example.csv", package = "ShuttleboxR"
#' )
#' project_data <- read_project_database(example_file)
#'
#' correlation_matrix(
#'   project_data,
#'   columns = c("Tpref", "Tavoid_lower", "Tavoid_upper", "nr_shuttles")
#' )
#'
#' @importFrom graphics pairs panel.smooth text
#' @importFrom stats aggregate cor
#' @export
correlation_matrix <- function(data,
                               columns,
                               exclude_start_minutes = 0,
                               exclude_end_minutes = 0,
                               interval_minutes = 10) {
  data <- .standardise_project_data(data)

  if (missing(columns) || length(columns) < 2L) {
    stop("`columns` must name at least two numeric columns.", call. = FALSE)
  }

  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "The following columns were not found: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  non_numeric <- columns[!vapply(data[columns], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    stop(
      "All selected columns must be numeric. Non-numeric columns: ",
      paste(non_numeric, collapse = ", "),
      call. = FALSE
    )
  }

  if ("time_sec" %in% names(data)) {
    time_sec <- suppressWarnings(as.numeric(data$time_sec))
    start_time <- exclude_start_minutes * 60
    end_time <- max(time_sec, na.rm = TRUE) - exclude_end_minutes * 60

    keep <- is.finite(time_sec) & time_sec >= start_time & time_sec <= end_time
    data_2 <- data[keep, , drop = FALSE]

    if (nrow(data_2) == 0L) {
      stop("No observations remain after the requested exclusions.", call. = FALSE)
    }

    data_2$t_interval <- floor(
      (as.numeric(data_2$time_sec) - start_time) / (interval_minutes * 60)
    ) * interval_minutes

    analysis_data <- stats::aggregate(
      data_2[, columns, drop = FALSE],
      by = list(t_interval = data_2$t_interval),
      FUN = function(x) mean(x, na.rm = TRUE)
    )[, columns, drop = FALSE]
  } else {
    analysis_data <- data[, columns, drop = FALSE]
  }

  complete_rows <- stats::complete.cases(analysis_data)
  analysis_data <- analysis_data[complete_rows, , drop = FALSE]

  if (nrow(analysis_data) < 3L) {
    stop("At least three complete observations are required.", call. = FALSE)
  }

  correlation_values <- stats::cor(analysis_data, use = "complete.obs")

  panel_cor <- function(x, y, digits = 2, cex.cor = 1.2, ...) {
    r <- stats::cor(x, y, use = "complete.obs")
    graphics::text(
      mean(range(x, finite = TRUE)),
      mean(range(y, finite = TRUE)),
      formatC(r, format = "f", digits = digits),
      cex = cex.cor
    )
  }

  graphics::pairs(
    analysis_data,
    lower.panel = graphics::panel.smooth,
    upper.panel = panel_cor
  )

  invisible(correlation_values)
}
