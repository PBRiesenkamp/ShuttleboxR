#' Calculate shuttle-box metrics for all trials
#'
#' Calculates standard ShuttleboxR metrics for every trial in a list, such as
#' the object returned by [read_shuttlesoft_project()].
#'
#' Gravitation time is estimated once per fish under the hood. When
#' `exclude_gravitation_thermal = TRUE`, the same checked gravitation duration
#' is used to remove the transitional period from Tpref, avoidance
#' temperatures, Tbreadth, percentile-based thermal range, core-temperature variation, and exposure near the
#' programmed limits. The duration is added to the start of the dynamic period
#' when `exclude_acclimation = TRUE`, or to the start of the complete recording
#' otherwise.
#'
#' Activity metrics can be treated separately with
#' `exclude_gravitation_activity`. This is `FALSE` by default because some
#' studies require total movement and shuttling over the complete selected
#' period rather than settled thermoregulation only.
#'
#' @param data_read A list containing imported shuttle-box trials.
#' @param calculate_distance Logical. Calculate distance from coordinates.
#'   Default is `FALSE`.
#' @param pixel_to_cm Logical. Convert pixels to centimetres when calculating
#'   distance. Default is `TRUE`.
#' @param recalculate_core_T Logical. Recalculate body temperature with
#'   [calc_coreT()]. Default is `FALSE`.
#' @param exclude_acclimation Logical. Use the dynamic period as the origin for
#'   calculations. Default is `FALSE`.
#' @param exclude_start_minutes Minutes omitted from the start of the selected
#'   origin. Default is 0.
#' @param exclude_end_minutes Minutes omitted from the end of each recording.
#'   Default is 0.
#' @param exclude_gravitation_thermal Logical. Exclude gravitation from thermal
#'   distribution metrics. Default is `FALSE` for backwards compatibility;
#'   `TRUE` is generally recommended after inspecting the segmented plots.
#' @param exclude_gravitation_activity Logical. Exclude gravitation from total
#'   distance, shuttle count, and chamber occupancy. Default is `FALSE`.
#' @param gravitation_failure What to do when gravitation cannot be estimated:
#'   `"warn"` records `NA` for metrics that require post-gravitation data, while
#'   `"error"` stops processing. Default is `"warn"`.
#' @param Tpref_method Method used by [calc_Tpref()]. Default is `"median"`.
#' @param Tavoid_percentiles Lower and upper percentiles used by
#'   [calc_Tavoid()]. Default is `c(0.05, 0.95)`.
#' @param Tpercentile_range_percentiles Lower and upper percentiles used by
#'   [calc_Tpercentile_range()]. Default is `c(0.25, 0.75)`.
#' @param textremes_threshold Definition of the extreme-temperature range.
#' @param core_T_variance_type Method used by [calc_coreT_variance()].
#'
#' @return A data frame containing one row of metrics per trial. Additional
#'   columns record the gravitation reference and the starts of the thermal and
#'   activity analysis windows.
#'
#' @export
calc_project_results <- function(
    data_read,
    calculate_distance = FALSE,
    pixel_to_cm = TRUE,
    recalculate_core_T = FALSE,
    exclude_acclimation = FALSE,
    exclude_start_minutes = 0,
    exclude_end_minutes = 0,
    Tpref_method = "median",
    Tavoid_percentiles = c(0.05, 0.95),
    Tpercentile_range_percentiles = c(0.25, 0.75),
    textremes_threshold = expression(0.2 * (max(df$max_T, na.rm = TRUE) - max(df$min_T, na.rm = TRUE))),
    core_T_variance_type = "std_error",
    exclude_gravitation_thermal = FALSE,
    exclude_gravitation_activity = FALSE,
    gravitation_failure = c("warn", "error")) {

  gravitation_failure <- match.arg(gravitation_failure)
  if (!is.list(data_read) || length(data_read) == 0L) {
    stop("`data_read` must be a non-empty list of shuttle-box data frames.", call. = FALSE)
  }

  .validate_nonnegative_number(exclude_start_minutes, "exclude_start_minutes")
  .validate_nonnegative_number(exclude_end_minutes, "exclude_end_minutes")

  message("Initialising...")
  data_list <- lapply(data_read, file_prepare)

  apply_functions <- function(df) {
    file_id <- unique(as.character(df$fileID))
    file_id <- file_id[!is.na(file_id) & nzchar(file_id)]
    file_id <- if (length(file_id) == 0L) NA_character_ else file_id[1L]

    if (isTRUE(recalculate_core_T)) {
      df <- calc_coreT(df)
    } else if (!"core_T" %in% names(df) || all(is.na(df$core_T))) {
      stop(
        "A trial has no usable `core_T`. Supply core temperatures or set `recalculate_core_T = TRUE` and provide calibrated parameters.",
        call. = FALSE
      )
    }

    if (isTRUE(calculate_distance)) {
      df <- calc_distance(df, pixel_to_cm)
    }

    textremes_th <- eval(textremes_threshold)
    reference <- if (isTRUE(exclude_acclimation)) "dynamic_period" else "recording"

    grav_error <- NULL
    grav_time <- tryCatch(
      calc_gravitation(
        df,
        exclude_start_minutes = 0,
        exclude_end_minutes = exclude_end_minutes,
        exclude_acclimation = exclude_acclimation,
        print_results = FALSE
      ),
      error = function(e) {
        grav_error <<- conditionMessage(e)
        NA_real_
      }
    )
    grav_valid <- is.finite(grav_time) && grav_time >= 0

    if (!grav_valid && (isTRUE(exclude_gravitation_thermal) ||
                        isTRUE(exclude_gravitation_activity))) {
      message_text <- paste0(
        "Gravitation time could not be estimated for ", file_id,
        ": ", grav_error,
        ". Metrics requiring post-gravitation observations will be recorded as NA."
      )
      if (identical(gravitation_failure, "error")) {
        stop(message_text, call. = FALSE)
      }
      warning(message_text, call. = FALSE)
    }

    thermal_available <- !isTRUE(exclude_gravitation_thermal) || grav_valid
    activity_available <- !isTRUE(exclude_gravitation_activity) || grav_valid

    thermal_args <- list(
      data = df,
      exclude_start_minutes = exclude_start_minutes,
      exclude_end_minutes = exclude_end_minutes,
      exclude_acclimation = exclude_acclimation,
      exclude_gravitation = exclude_gravitation_thermal,
      gravitation_time = if (grav_valid) grav_time else NULL
    )
    activity_args <- list(
      data = df,
      exclude_start_minutes = exclude_start_minutes,
      exclude_end_minutes = exclude_end_minutes,
      exclude_acclimation = exclude_acclimation,
      exclude_gravitation = exclude_gravitation_activity,
      gravitation_time = if (grav_valid) grav_time else NULL
    )

    if (thermal_available) {
      Tpref <- do.call(calc_Tpref, c(
        thermal_args,
        list(method = Tpref_method, print_results = FALSE)
      ))
      Tavoid <- do.call(calc_Tavoid, c(
        thermal_args,
        list(percentiles = Tavoid_percentiles, print_results = FALSE)
      ))
      Tpref_range <- Tavoid[2L] - Tavoid[1L]
      Tbreadth <- do.call(calc_Tbreadth, c(
        thermal_args,
        list(print_results = FALSE)
      ))
      Tpercentile_range <- do.call(calc_Tpercentile_range, c(
        thermal_args,
        list(
          percentiles = Tpercentile_range_percentiles,
          print_results = FALSE
        )
      ))
      textremes <- do.call(calc_extremes, c(
        thermal_args,
        list(threshold = textremes_th, print_results = FALSE)
      ))
      core_T_variance <- do.call(calc_coreT_variance, c(
        thermal_args,
        list(variance_type = core_T_variance_type)
      ))
    } else {
      Tpref <- Tpref_range <- Tbreadth <- Tpercentile_range <- core_T_variance <- NA_real_
      Tavoid <- c(lower = NA_real_, upper = NA_real_)
      textremes <- c(lower = NA_real_, upper = NA_real_)
    }

    if (activity_available) {
      tot_distance <- do.call(calc_tot_distance, c(
        activity_args,
        list(print_results = FALSE)
      ))
      nr_shuttles <- do.call(calc_shuttles, c(
        activity_args,
        list(print_results = FALSE)
      ))
      chamber_seconds <- do.call(calc_occupancy, c(
        activity_args,
        list(print_results = FALSE)
      ))
    } else {
      tot_distance <- nr_shuttles <- NA_real_
      chamber_seconds <- c(DECR = NA_real_, INCR = NA_real_)
    }

    track_accuracy <- calc_track_accuracy(
      df,
      exclude_start_minutes = exclude_start_minutes,
      exclude_end_minutes = exclude_end_minutes,
      exclude_acclimation = exclude_acclimation,
      print_results = FALSE
    )

    thermal_start_h <- if (isTRUE(exclude_gravitation_thermal)) {
      if (grav_valid) max(exclude_start_minutes / 60, grav_time) else NA_real_
    } else {
      exclude_start_minutes / 60
    }
    activity_start_h <- if (isTRUE(exclude_gravitation_activity)) {
      if (grav_valid) max(exclude_start_minutes / 60, grav_time) else NA_real_
    } else {
      exclude_start_minutes / 60
    }

    data.frame(
      fileID = file_id,
      Tpref = Tpref,
      Tpref_range = Tpref_range,
      Tbreadth = Tbreadth,
      Tpercentile_range = Tpercentile_range,
      grav_time = grav_time,
      gravitation_valid = grav_valid,
      gravitation_reference = reference,
      thermal_metrics_start_h = thermal_start_h,
      activity_metrics_start_h = activity_start_h,
      thermal_metrics_post_gravitation = isTRUE(exclude_gravitation_thermal),
      activity_metrics_post_gravitation = isTRUE(exclude_gravitation_activity),
      tot_distance = tot_distance,
      Tavoid_lower = Tavoid[1L],
      Tavoid_upper = Tavoid[2L],
      t_near_min = textremes[1L],
      t_near_max = textremes[2L],
      t_near_limits = sum(textremes),
      track_accuracy = track_accuracy,
      core_T_variance = core_T_variance,
      nr_shuttles = nr_shuttles,
      seconds_in_DECR = chamber_seconds[1L],
      seconds_in_INCR = chamber_seconds[2L],
      stringsAsFactors = FALSE
    )
  }

  results <- do.call(
    rbind,
    lapply(seq_along(data_list), function(i) {
      message(
        "Processing dataset ", i, " of ", length(data_list),
        " (", round(i / length(data_list) * 100, 2), "% complete)"
      )
      apply_functions(data_list[[i]])
    })
  )

  rownames(results) <- NULL
  results
}
