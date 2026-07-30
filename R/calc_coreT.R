#' Recalculate core body temperature
#'
#' Recalculates core body temperature from ambient chamber temperature and a
#' calibrated thermal-lag model. Most ShuttleSoft files already contain
#' `core_T`, so this function is optional.
#'
#' The model uses `k = a_value * mass^b_value`. The `a_value` and `b_value`
#' coefficients must come from an appropriate calibration or published source;
#' they cannot be inferred from the ShuttleSoft file itself.
#'
#' Values may be supplied directly as arguments or stored in columns of the
#' same names. Direct arguments take priority. When `initial_T` is omitted, the
#' first valid existing `core_T` value is used where possible.
#'
#' @param data An organised shuttle-box data frame.
#' @param mass Optional body mass.
#' @param initial_T Optional initial body temperature.
#' @param a_value Optional calibrated coefficient `a`.
#' @param b_value Optional calibrated coefficient `b`.
#'
#' @return The data frame with recalculated `core_T`, `ambient_T`, and `k`.
#'
#' @examples
#' \dontrun{
#' fish <- calc_coreT(
#'   fish,
#'   mass = 12.4,
#'   a_value = 0.05,
#'   b_value = -0.25
#' )
#' }
#'
#' @export
calc_coreT <- function(data,
                       mass = NULL,
                       initial_T = NULL,
                       a_value = NULL,
                       b_value = NULL) {

  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }

  if (!"shuttle" %in% names(data)) {
    data <- file_prepare(data)
  }

  resolve_value <- function(argument, column_name) {
    if (!is.null(argument) && length(argument) > 0L && !all(is.na(argument))) {
      return(argument[[1L]])
    }

    if (column_name %in% names(data)) {
      values <- unique(data[[column_name]][!is.na(data[[column_name]])])
      if (length(values) > 1L) {
        stop(
          "`", column_name, "` contains more than one value. Supply one value directly.",
          call. = FALSE
        )
      }
      if (length(values) == 1L) {
        return(values[[1L]])
      }
    }

    NA_real_
  }

  mass_value <- resolve_value(mass, "mass")
  a_value_resolved <- resolve_value(a_value, "a_value")
  b_value_resolved <- resolve_value(b_value, "b_value")
  initial_value <- resolve_value(initial_T, "initial_T")

  if (!is.finite(suppressWarnings(as.numeric(initial_value))) &&
      "core_T" %in% names(data)) {
    existing_core <- suppressWarnings(as.numeric(data$core_T))
    valid_core <- existing_core[is.finite(existing_core)]
    if (length(valid_core) > 0L) {
      initial_value <- valid_core[1L]
    }
  }

  parameter_values <- c(
    mass = suppressWarnings(as.numeric(mass_value)),
    initial_T = suppressWarnings(as.numeric(initial_value)),
    a_value = suppressWarnings(as.numeric(a_value_resolved)),
    b_value = suppressWarnings(as.numeric(b_value_resolved))
  )

  if (any(!is.finite(parameter_values))) {
    missing_names <- names(parameter_values)[!is.finite(parameter_values)]
    stop(
      "To recalculate `core_T`, provide: ",
      paste(missing_names, collapse = ", "),
      ". Supply them as arguments or data columns. If ShuttleSoft already supplied `core_T`, you do not need to run `calc_coreT()`.",
      call. = FALSE
    )
  }

  if (parameter_values["mass"] <= 0) {
    stop("`mass` must be greater than zero.", call. = FALSE)
  }

  required_columns <- c("zone", "INCR_T", "DECR_T", "shuttle")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "The data are missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  data$INCR_T <- suppressWarnings(as.numeric(data$INCR_T))
  data$DECR_T <- suppressWarnings(as.numeric(data$DECR_T))
  data$shuttle <- suppressWarnings(as.numeric(data$shuttle))

  k_value <- parameter_values["a_value"] *
    parameter_values["mass"]^parameter_values["b_value"]

  if (!is.finite(k_value) || k_value <= 0) {
    stop("The supplied values produce a non-positive or invalid thermal constant `k`.", call. = FALSE)
  }

  data$mass <- unname(parameter_values["mass"])
  data$initial_T <- unname(parameter_values["initial_T"])
  data$a_value <- unname(parameter_values["a_value"])
  data$b_value <- unname(parameter_values["b_value"])
  data$k <- unname(k_value)
  data$ambient_T <- ifelse(data$zone == "INCR", data$INCR_T, data$DECR_T)

  data$core_T <- NA_real_
  data$core_T[1L] <- unname(parameter_values["initial_T"])

  if (nrow(data) >= 2L) {
    for (i in 2:nrow(data)) {
      if (isTRUE(data$shuttle[i] == 1)) {
        data$core_T[i] <- data$core_T[i - 1L]
      } else {
        data$core_T[i] <- data$ambient_T[i] +
          (data$core_T[i - 1L] - data$ambient_T[i]) *
          exp(-k_value / 60)
      }
    }
  }

  data
}
