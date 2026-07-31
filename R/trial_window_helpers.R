# Internal helpers for defining analysis windows ----------------------------

.validate_nonnegative_number <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) {
    stop("`", name, "` must be one finite number that is zero or greater.", call. = FALSE)
  }
  invisible(TRUE)
}

.trial_origin_seconds <- function(data, exclude_acclimation = FALSE) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  if (!"time_sec" %in% names(data)) {
    stop("The dataset does not contain `time_sec`.", call. = FALSE)
  }

  time_sec <- suppressWarnings(as.numeric(data$time_sec))
  if (!any(is.finite(time_sec))) {
    stop("`time_sec` contains no finite values.", call. = FALSE)
  }

  if (isTRUE(exclude_acclimation)) {
    if (!"trial_phase" %in% names(data)) {
      stop(
        "To use the dynamic-period start, provide `trial_start` when importing so that `trial_phase` is available.",
        call. = FALSE
      )
    }
    phase <- as.character(data$trial_phase)
    eligible <- !is.na(phase) & phase != "acclimation" & is.finite(time_sec)
    if (!any(eligible)) {
      stop("No dynamic-period observations are available.", call. = FALSE)
    }
    return(min(time_sec[eligible], na.rm = TRUE))
  }

  min(time_sec, na.rm = TRUE)
}

.fit_gravitation_model <- function(data,
                                   exclude_start_minutes = 0,
                                   exclude_end_minutes = 0,
                                   exclude_acclimation = FALSE) {
  .validate_nonnegative_number(exclude_start_minutes, "exclude_start_minutes")
  .validate_nonnegative_number(exclude_end_minutes, "exclude_end_minutes")

  if (!all(c("time_sec", "core_T") %in% names(data))) {
    stop("The dataset must contain `time_sec` and `core_T`.", call. = FALSE)
  }

  time_sec <- suppressWarnings(as.numeric(data$time_sec))
  core_T <- suppressWarnings(as.numeric(data$core_T))
  origin_sec <- .trial_origin_seconds(data, exclude_acclimation)
  recording_end <- max(time_sec[is.finite(time_sec)], na.rm = TRUE)

  fit_start <- origin_sec + exclude_start_minutes * 60
  fit_end <- recording_end - exclude_end_minutes * 60
  if (fit_end <= fit_start) {
    stop("The requested exclusions leave no time window for gravitation analysis.", call. = FALSE)
  }

  keep <- is.finite(time_sec) & is.finite(core_T) &
    time_sec >= fit_start & time_sec <= fit_end

  if (isTRUE(exclude_acclimation)) {
    phase <- as.character(data$trial_phase)
    keep <- keep & !is.na(phase) & phase != "acclimation"
  }

  fit_data <- data[keep, , drop = FALSE]
  fit_data$time_from_origin_h <- (time_sec[keep] - origin_sec) / 3600
  fit_data$core_T_numeric <- core_T[keep]

  if (nrow(fit_data) < 10L) {
    stop("Too few observations remain to estimate gravitation time.", call. = FALSE)
  }
  if (length(unique(fit_data$time_from_origin_h)) < 4L ||
      length(unique(fit_data$core_T_numeric)) < 3L) {
    stop("The selected data do not contain enough variation for segmented regression.", call. = FALSE)
  }

  linear_fit <- stats::lm(core_T_numeric ~ time_from_origin_h, data = fit_data)
  segmented_fit <- tryCatch(
    segmented::segmented(
      linear_fit,
      seg.Z = ~time_from_origin_h,
      npsi = 1
    ),
    error = function(e) {
      stop(
        "Gravitation time could not be estimated: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  breakpoint_h <- as.numeric(segmented_fit$psi[1L, "Est."])
  if (!is.finite(breakpoint_h) || breakpoint_h < 0) {
    stop("The segmented model did not return a valid gravitation breakpoint.", call. = FALSE)
  }

  cutoff_sec <- origin_sec + breakpoint_h * 3600
  if (cutoff_sec < fit_start || cutoff_sec > fit_end) {
    stop("The estimated gravitation breakpoint lies outside the analysed time window.", call. = FALSE)
  }

  list(
    fit_data = fit_data,
    linear_fit = linear_fit,
    segmented_fit = segmented_fit,
    origin_sec = origin_sec,
    breakpoint_h = breakpoint_h,
    cutoff_sec = cutoff_sec,
    fit_start_sec = fit_start,
    fit_end_sec = fit_end
  )
}

.prepare_trial_window <- function(data,
                                  exclude_start_minutes = 0,
                                  exclude_end_minutes = 0,
                                  exclude_acclimation = FALSE,
                                  exclude_gravitation = FALSE,
                                  gravitation_time = NULL,
                                  context = "calculation") {
  .validate_nonnegative_number(exclude_start_minutes, "exclude_start_minutes")
  .validate_nonnegative_number(exclude_end_minutes, "exclude_end_minutes")

  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  if (!"time_sec" %in% names(data)) {
    data$time_sec <- seq.int(0L, nrow(data) - 1L)
  }

  time_sec <- suppressWarnings(as.numeric(data$time_sec))
  if (!any(is.finite(time_sec))) {
    stop("`time_sec` contains no finite values.", call. = FALSE)
  }

  origin_sec <- .trial_origin_seconds(data, exclude_acclimation)
  recording_end <- max(time_sec[is.finite(time_sec)], na.rm = TRUE)

  analysis_start <- origin_sec + exclude_start_minutes * 60
  analysis_end <- recording_end - exclude_end_minutes * 60
  resolved_gravitation <- NA_real_
  gravitation_cutoff <- NA_real_

  if (isTRUE(exclude_gravitation)) {
    if (is.null(gravitation_time)) {
      resolved_gravitation <- calc_gravitation(
        data,
        exclude_start_minutes = 0,
        exclude_end_minutes = exclude_end_minutes,
        exclude_acclimation = exclude_acclimation,
        print_results = FALSE
      )
    } else {
      .validate_nonnegative_number(gravitation_time, "gravitation_time")
      resolved_gravitation <- as.numeric(gravitation_time)
    }

    gravitation_cutoff <- origin_sec + resolved_gravitation * 3600
    analysis_start <- max(analysis_start, gravitation_cutoff)
  }

  if (analysis_end < analysis_start) {
    stop(
      "No observations remain for ", context,
      " after applying the requested acclimation, gravitation, and time exclusions.",
      call. = FALSE
    )
  }

  keep <- is.finite(time_sec) & time_sec >= analysis_start & time_sec <= analysis_end
  if (isTRUE(exclude_acclimation)) {
    if (!"trial_phase" %in% names(data)) {
      stop(
        "To exclude acclimation, provide `trial_start` when importing so that `trial_phase` is available.",
        call. = FALSE
      )
    }
    phase <- as.character(data$trial_phase)
    keep <- keep & !is.na(phase) & phase != "acclimation"
  }

  result <- data[keep, , drop = FALSE]
  if (nrow(result) == 0L) {
    stop("No observations remain for ", context, ".", call. = FALSE)
  }

  attr(result, "analysis_origin_sec") <- origin_sec
  attr(result, "analysis_start_sec") <- analysis_start
  attr(result, "analysis_end_sec") <- analysis_end
  attr(result, "gravitation_time_h") <- resolved_gravitation
  attr(result, "gravitation_cutoff_sec") <- gravitation_cutoff
  result
}
