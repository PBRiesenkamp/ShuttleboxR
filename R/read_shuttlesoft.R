#' Import a ShuttleSoft data file
#'
#' Imports a single ShuttleSoft tab-delimited `.txt` file or comma-separated
#' `.csv` export. A metadata table is optional. For a single trial, values such
#' as the trial start time can be supplied directly as arguments. By default,
#' the imported file is prepared for analysis with [file_prepare()].
#'
#' ShuttleSoft files normally already contain a `core_T` column. The arguments
#' `mass`, `initial_T`, `a_value`, and `b_value` are therefore optional and are
#' only needed if body temperature will later be recalculated with
#' [calc_coreT()]. Direct arguments take priority over values in `metadata`.
#'
#' @param file Path to one ShuttleSoft `.txt` or `.csv` file. Use
#'   `file.choose()` to select a file interactively.
#' @param metadata Optional data frame containing one row per file. It must
#'   contain `file_name` and may contain `trial_start`, `mass`, `initial_T`,
#'   `a_value`, and `b_value`. The legacy name `initial_temp` is also accepted.
#' @param multidat Deprecated compatibility argument. It is no longer needed.
#' @param trial_start Optional clock time at which the experimental trial began,
#'   written as `"HH:MM:SS"`. When omitted, the first observation is treated as
#'   the start of the trial, so the whole recording is labelled as trial data.
#' @param mass Optional body mass used only when recalculating `core_T`.
#' @param initial_T Optional initial body temperature used only when
#'   recalculating `core_T`.
#' @param a_value Optional calibrated coefficient used only when recalculating
#'   `core_T`.
#' @param b_value Optional calibrated coefficient used only when recalculating
#'   `core_T`.
#' @param prepare Logical. If `TRUE` (the default), run [file_prepare()] before
#'   returning the data.
#'
#' @return A ShuttleSoft data frame. When `prepare = TRUE`, it is ready for
#'   calculation and plotting functions.
#'
#' @examples
#' example_file <- system.file(
#'   "extdata", "Fish_7_13_2.csv",
#'   package = "ShuttleboxR"
#' )
#' fish <- read_shuttlesoft(example_file)
#' calc_Tpref(fish, print_results = FALSE)
#'
#' @seealso [read_shuttlesoft_project()], [file_prepare()], [calc_coreT()]
#' @export
read_shuttlesoft <- function(file,
                             metadata = NULL,
                             multidat = FALSE,
                             trial_start = NULL,
                             mass = NULL,
                             initial_T = NULL,
                             a_value = NULL,
                             b_value = NULL,
                             prepare = TRUE) {

  if (length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("`file` must be the path to one ShuttleSoft file.", call. = FALSE)
  }

  if (!file.exists(file)) {
    stop("The file does not exist: ", file, call. = FALSE)
  }

  column_names <- c(
    "time", "zone", "core_T", "Tpref_loligo", "INCR_T", "DECR_T",
    "x_pos", "y_pos", "velocity", "distance", "time_in_INCR",
    "time_in_DECR", "delta_T", "dyn_hysteresis", "stat_T_INCR",
    "stat_hyst_INCR", "stat_T_DECR", "stat_hyst_DECR", "k", "max_T",
    "min_T", "change_rate", "avoidance_upper", "avoidance_upper_core",
    "avoidance_lower", "avoidance_lower_core"
  )

  extension <- tolower(tools::file_ext(file))

  if (extension == "csv") {
    data <- utils::read.csv(
      file,
      header = FALSE,
      col.names = column_names,
      na.strings = c("NaN", ""),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    data <- utils::read.delim(
      file,
      header = FALSE,
      col.names = column_names,
      na.strings = c("NaN", ""),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  first_value <- function(x, default = NA_character_) {
    if (length(x) == 0L || all(is.na(x))) {
      return(default)
    }
    x[which(!is.na(x))[1L]]
  }

  notes <- first_value(data$zone[data$time == "Notes"])
  file_created_text <- first_value(data$zone[data$time == "File created"])
  pixel_ratio <- first_value(data$zone[data$time == "Pixel Ratio [cm/pix]"])

  file_created <- as.POSIXct(
    file_created_text,
    format = "%d/%m/%Y; %H:%M",
    tz = ""
  )

  data <- data[!is.na(data$time), , drop = FALSE]
  data <- data[!is.na(data$INCR_T), , drop = FALSE]
  numeric_rows <- !is.na(suppressWarnings(as.numeric(data$INCR_T)))
  data <- data[numeric_rows, , drop = FALSE]
  rownames(data) <- NULL

  if (nrow(data) == 0L) {
    stop("No ShuttleSoft observations could be read from this file.", call. = FALSE)
  }

  data$notes <- notes
  data$pixel_ratio <- pixel_ratio
  data$date <- as.Date(file_created)
  file_id <- basename(file)
  data$fileID <- file_id

  metadata_row <- NULL
  if (!is.null(metadata)) {
    if (!is.data.frame(metadata)) {
      stop("`metadata` must be a data frame when supplied.", call. = FALSE)
    }
    if (!"file_name" %in% names(metadata)) {
      stop("`metadata` must contain a `file_name` column.", call. = FALSE)
    }

    matched_rows <- which(as.character(metadata$file_name) == file_id)
    if (length(matched_rows) == 0L) {
      warning(
        "No metadata row matched ", file_id,
        "; direct arguments and file values will be used.",
        call. = FALSE
      )
    } else {
      if (length(matched_rows) > 1L) {
        warning(
          "More than one metadata row matched ", file_id,
          "; the first match will be used.",
          call. = FALSE
        )
      }
      metadata_row <- metadata[matched_rows[1L], , drop = FALSE]
    }
  }

  metadata_value <- function(argument, possible_names, default = NA) {
    if (!is.null(argument) && length(argument) > 0L && !all(is.na(argument))) {
      return(argument[[1L]])
    }

    if (!is.null(metadata_row)) {
      for (column_name in possible_names) {
        if (column_name %in% names(metadata_row)) {
          value <- metadata_row[[column_name]][1L]
          if (length(value) > 0L && !is.na(value)) {
            return(value)
          }
        }
      }
    }

    default
  }

  trial_start_value <- metadata_value(
    trial_start,
    "trial_start",
    default = as.character(data$time[1L])
  )

  if (inherits(trial_start_value, c("POSIXct", "POSIXlt"))) {
    trial_start_value <- format(trial_start_value, "%H:%M:%S")
  } else {
    trial_start_value <- trimws(as.character(trial_start_value))
  }

  if (grepl("^\\d{1,2}:\\d{2}$", trial_start_value)) {
    trial_start_value <- paste0(trial_start_value, ":00")
  }

  if (!grepl("^\\d{1,2}:\\d{2}:\\d{2}$", trial_start_value)) {
    warning(
      "`trial_start` is not in HH:MM:SS format; the first observation will be used.",
      call. = FALSE
    )
    trial_start_value <- as.character(data$time[1L])
  }

  data$trial_start <- trial_start_value
  data$mass <- metadata_value(mass, "mass", default = NA_real_)
  data$initial_T <- metadata_value(
    initial_T,
    c("initial_T", "initial_temp"),
    default = NA_real_
  )
  data$a_value <- metadata_value(a_value, "a_value", default = NA_real_)
  data$b_value <- metadata_value(b_value, "b_value", default = NA_real_)

  if (isTRUE(prepare)) {
    data <- file_prepare(data)
  }

  data
}
