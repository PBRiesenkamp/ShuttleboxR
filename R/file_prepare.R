#' Prepare shuttle-box data
#'
#' Adds elapsed time, date-time, trial phase, dynamic/static state, and shuttle
#' events to a raw ShuttleSoft data frame. The function can safely be run on a
#' data frame that has already been prepared.
#'
#' When no valid `trial_start` is available, the first observation is used and
#' the whole recording is labelled as `"trial"`.
#'
#' @param data A raw shuttle-box data frame, for example as returned by
#'   [read_shuttlesoft()].
#'
#' @return An organised shuttle-box data frame ready for calculation functions.
#'
#' @export
file_prepare <- function(data) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop("`data` contains no observations.", call. = FALSE)
  }

  data$time_sec <- seq.int(0L, nrow(data) - 1L)
  data$time_h <- data$time_sec / 3600

  if (all(c("x_pos", "y_pos") %in% names(data))) {
    data$x_pos[data$x_pos == "No object"] <- NA
    data$y_pos[data$y_pos == "No object"] <- NA
    data$x_pos <- suppressWarnings(as.numeric(data$x_pos))
    data$y_pos <- suppressWarnings(as.numeric(data$y_pos))
  }

  if (!"date" %in% names(data)) {
    data$date <- as.Date(rep(NA_character_, nrow(data)))
  }

  if (length(unique(data$date[!is.na(data$date)])) <= 1L && "time" %in% names(data)) {
    midnight_rows <- which(as.character(data$time) == "00:00:00")
    if (length(midnight_rows) > 0L && midnight_rows[1L] > 1L) {
      data$date[midnight_rows[1L]:nrow(data)] <-
        data$date[midnight_rows[1L]:nrow(data)] + 1
    }
  }

  if (all(c("date", "time") %in% names(data))) {
    data$datetime <- as.POSIXct(
      paste(data$date, data$time),
      format = "%Y-%m-%d %H:%M:%S",
      tz = ""
    )
  } else {
    data$datetime <- as.POSIXct(rep(NA_character_, nrow(data)))
  }

  if ("delta_T" %in% names(data)) {
    data$dyn_stat <- ifelse(is.na(data$delta_T), "static", "dynamic")
  }

  time_to_seconds <- function(x) {
    pieces <- strsplit(as.character(x), ":", fixed = TRUE)[[1L]]
    if (length(pieces) != 3L) {
      return(NA_real_)
    }
    values <- suppressWarnings(as.numeric(pieces))
    if (any(!is.finite(values))) {
      return(NA_real_)
    }
    values[1L] * 3600 + values[2L] * 60 + values[3L]
  }

  trial_start <- NA_character_
  if ("trial_start" %in% names(data)) {
    possible_starts <- unique(as.character(data$trial_start))
    possible_starts <- possible_starts[!is.na(possible_starts) & nzchar(possible_starts)]
    if (length(possible_starts) > 0L) {
      trial_start <- possible_starts[1L]
    }
  }

  if (is.na(trial_start) || !"time" %in% names(data)) {
    trial_start_second <- 0
  } else {
    recorded_seconds <- vapply(data$time, time_to_seconds, numeric(1L))

    if (length(recorded_seconds) > 1L) {
      for (i in 2:length(recorded_seconds)) {
        if (is.finite(recorded_seconds[i]) && is.finite(recorded_seconds[i - 1L]) &&
            recorded_seconds[i] < recorded_seconds[i - 1L]) {
          recorded_seconds[i:length(recorded_seconds)] <-
            recorded_seconds[i:length(recorded_seconds)] + 24 * 3600
          break
        }
      }
    }

    target_seconds <- time_to_seconds(trial_start)
    if (is.finite(target_seconds) && is.finite(recorded_seconds[1L]) &&
        target_seconds < recorded_seconds[1L] &&
        any(recorded_seconds >= 24 * 3600, na.rm = TRUE)) {
      target_seconds <- target_seconds + 24 * 3600
    }

    trial_row <- which(is.finite(recorded_seconds) & recorded_seconds >= target_seconds)[1L]

    if (length(trial_row) == 0L || is.na(trial_row) || !is.finite(target_seconds)) {
      warning(
        "`trial_start` was not found in the recording; the first observation will be used.",
        call. = FALSE
      )
      trial_start_second <- 0
    } else {
      trial_start_second <- data$time_sec[trial_row]
    }
  }

  data$trial_phase <- ifelse(
    data$time_sec >= trial_start_second,
    "trial",
    "acclimation"
  )

  if ("zone" %in% names(data)) {
    zone_changed <- c(FALSE, data$zone[-1L] != data$zone[-nrow(data)])
    zone_changed[is.na(zone_changed)] <- FALSE
    data$shuttle <- as.integer(zone_changed)
  } else {
    data$shuttle <- 0L
  }

  if (!"distance" %in% names(data) &&
      !all(c("x_pos", "y_pos") %in% names(data))) {
    warning(
      "The dataset has no `distance` column or x/y coordinates; an empty `distance` column was added.",
      call. = FALSE
    )
    data$distance <- NA_real_
  }

  if (!all(c("max_T", "min_T") %in% names(data))) {
    warning(
      "The dataset has no `min_T` and/or `max_T` columns; empty columns were added.",
      call. = FALSE
    )
    if (!"min_T" %in% names(data)) data$min_T <- NA_real_
    if (!"max_T" %in% names(data)) data$max_T <- NA_real_
  }

  data <- utils::type.convert(data, as.is = TRUE)
  data$datetime <- as.POSIXct(data$datetime, origin = "1970-01-01", tz = "")
  data
}
