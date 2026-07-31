#' Calculate shuttle-box metrics for all trials
#'
#' Calculates the standard ShuttleboxR metrics for every trial in a list, such
#' as the object returned by [read_shuttlesoft_project()]. The output now
#' includes selected thermal breadth from [calc_Tbreadth()], calculated as
#' the mean pairwise difference among observed core temperatures.
#'
#' ShuttleSoft files normally already contain `core_T`, so recalculation is off
#' by default. Set `recalculate_core_T = TRUE` only when calibrated thermal-lag
#' parameters are available for every trial.
#'
#' @param data_read A list containing imported shuttle-box trials.
#' @param calculate_distance Logical. Calculate distance from coordinates.
#'   Default is `FALSE`.
#' @param pixel_to_cm Logical. Convert pixels to centimetres when calculating
#'   distance. Default is `TRUE`.
#' @param recalculate_core_T Logical. Recalculate body temperature with
#'   [calc_coreT()]. Default is `FALSE`, which uses the `core_T` already present
#'   in the ShuttleSoft files.
#' @param exclude_acclimation Logical. Exclude the acclimation period. Default
#'   is `FALSE`.
#' @param exclude_start_minutes Minutes excluded from the start of each
#'   recording. Default is 0.
#' @param exclude_end_minutes Minutes excluded from the end of each recording.
#'   Default is 0.
#' @param Tpref_method Method used by [calc_Tpref()]: `"median"`, `"mean"`, or
#'   `"mode"`. Default is `"median"`.
#' @param Tavoid_percentiles Lower and upper percentiles used by
#'   [calc_Tavoid()]. Default is `c(0.05, 0.95)`.
#' @param textremes_threshold Definition of the extreme-temperature range.
#' @param core_T_variance_type Method used by [calc_coreT_variance()].
#'
#' @return A data frame containing metrics for all trials.
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
    textremes_threshold = expression(0.2 * (max(df$max_T) - max(df$min_T))),
    core_T_variance_type = "std_error") {

  if (!is.list(data_read) || length(data_read) == 0L) {
    stop("`data_read` must be a non-empty list of shuttle-box data frames.", call. = FALSE)
  }

  message("Initialising...")
  data_list <- lapply(data_read, file_prepare)

  apply_functions <- function(df) {
    file_id <- unique(df$fileID)
    if (length(file_id) == 0L) file_id <- NA_character_
    if (length(file_id) > 1L) file_id <- file_id[1L]

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

    if (isTRUE(exclude_acclimation)) {
      df <- df[df$trial_phase != "acclimation", , drop = FALSE]
    }

    if (nrow(df) == 0L) {
      stop("No observations remain after excluding acclimation.", call. = FALSE)
    }

    end_time <- max(df$time_sec, na.rm = TRUE) - exclude_end_minutes * 60
    start_time <- exclude_start_minutes * 60
    df <- df[
      is.finite(df$time_sec) & df$time_sec >= start_time & df$time_sec <= end_time,
      ,
      drop = FALSE
    ]

    if (nrow(df) == 0L) {
      stop("No observations remain after the requested time exclusions.", call. = FALSE)
    }

    textremes_th <- eval(textremes_threshold)

    Tpref <- calc_Tpref(df, method = Tpref_method, print_results = FALSE)
    grav_time <- calc_gravitation(df, print_results = FALSE)
    tot_distance <- calc_tot_distance(df, print_results = FALSE)
    Tavoid <- calc_Tavoid(
      df,
      percentiles = Tavoid_percentiles,
      print_results = FALSE
    )
    Tpref_range <- Tavoid[2L] - Tavoid[1L]
    Tbreadth <- calc_Tbreadth(
      df,
      print_results = FALSE
    )
    textremes <- calc_extremes(
      df,
      threshold = textremes_th,
      print_results = FALSE
    )
    textremes_tot <- textremes[1L] + textremes[2L]
    track_accuracy <- calc_track_accuracy(df, print_results = FALSE)
    core_T_variance <- calc_coreT_variance(
      df,
      variance_type = core_T_variance_type
    )
    nr_shuttles <- calc_shuttles(df, print_results = FALSE)
    chamber_seconds <- calc_occupancy(df, print_results = FALSE)

    list(
      fileID = file_id,
      Tpref = Tpref,
      Tpref_range = Tpref_range,
      Tbreadth = Tbreadth,
      grav_time = grav_time,
      tot_distance = tot_distance,
      Tavoid = Tavoid,
      textremes = textremes,
      textremes_tot = textremes_tot,
      track_accuracy = track_accuracy,
      core_T_variance = core_T_variance,
      nr_shuttles = nr_shuttles,
      chamber_seconds = chamber_seconds
    )
  }

  results <- do.call(
    rbind,
    lapply(seq_along(data_list), function(i) {
      message(
        "Processing dataset ", i, " of ", length(data_list),
        " (", round(i / length(data_list) * 100, 2), "% complete)"
      )

      function_results <- apply_functions(data_list[[i]])

      data.frame(
        fileID = function_results$fileID,
        Tpref = function_results$Tpref,
        Tpref_range = function_results$Tpref_range,
        Tbreadth = function_results$Tbreadth,
        grav_time = function_results$grav_time,
        tot_distance = function_results$tot_distance,
        Tavoid_lower = function_results$Tavoid[1L],
        Tavoid_upper = function_results$Tavoid[2L],
        t_near_min = function_results$textremes[1L],
        t_near_max = function_results$textremes[2L],
        t_near_limits = function_results$textremes_tot,
        track_accuracy = function_results$track_accuracy,
        core_T_variance = function_results$core_T_variance,
        nr_shuttles = function_results$nr_shuttles,
        seconds_in_DECR = function_results$chamber_seconds[1L],
        seconds_in_INCR = function_results$chamber_seconds[2L]
      )
    })
  )

  rownames(results) <- NULL
  results
}
