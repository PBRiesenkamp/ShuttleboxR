# Standardise project-level column names ------------------------------------

.standardise_project_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  aliases <- c(
    study_ID = "fileID",
    distance = "tot_distance",
    shuttles = "nr_shuttles",
    pref_range = "Tpref_range",
    time_near_max = "t_near_max",
    time_near_min = "t_near_min",
    time_near_limits = "t_near_limits"
  )

  for (old_name in names(aliases)) {
    new_name <- unname(aliases[[old_name]])
    if (old_name %in% names(data) && !new_name %in% names(data)) {
      names(data)[names(data) == old_name] <- new_name
    }
  }

  if (!"fileID" %in% names(data) && "ID" %in% names(data)) {
    data$fileID <- as.character(data$ID)
  }

  if (!"Tpref_range" %in% names(data) &&
      all(c("Tavoid_lower", "Tavoid_upper") %in% names(data))) {
    data$Tpref_range <- data$Tavoid_upper - data$Tavoid_lower
  }

  data
}

# Project-level screening helpers -----------------------------------------

.validate_positive_scalar <- function(x, name, allow_zero = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop("`", name, "` must be one finite number.", call. = FALSE)
  }
  if ((isTRUE(allow_zero) && x < 0) || (!isTRUE(allow_zero) && x <= 0)) {
    comparison <- if (isTRUE(allow_zero)) "zero or greater" else "greater than zero"
    stop("`", name, "` must be ", comparison, ".", call. = FALSE)
  }
  invisible(TRUE)
}

.validate_review_quantiles <- function(lower_quantile, upper_quantile) {
  .validate_positive_scalar(lower_quantile, "lower_quantile", allow_zero = TRUE)
  .validate_positive_scalar(upper_quantile, "upper_quantile", allow_zero = TRUE)
  if (lower_quantile >= upper_quantile || upper_quantile > 1) {
    stop(
      "`lower_quantile` and `upper_quantile` must satisfy 0 <= lower < upper <= 1.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.classify_distance_shuttles <- function(data, cutoffs) {
  distance_state <- ifelse(
    data$tot_distance <= cutoffs[["distance_low"]], "low",
    ifelse(data$tot_distance >= cutoffs[["distance_high"]], "high", "typical")
  )
  shuttle_state <- ifelse(
    data$nr_shuttles <= cutoffs[["shuttles_low"]], "low",
    ifelse(data$nr_shuttles >= cutoffs[["shuttles_high"]], "high", "typical")
  )

  result <- rep("Typical project range", nrow(data))
  result[distance_state == "low" & shuttle_state == "low"] <-
    "Low movement + low shuttling"
  result[distance_state == "low" & shuttle_state == "high"] <-
    "Low movement + high shuttling"
  result[distance_state == "high" & shuttle_state == "low"] <-
    "High movement + low shuttling"
  result[distance_state == "high" & shuttle_state == "high"] <-
    "High movement + high shuttling"
  result[distance_state == "low" & shuttle_state == "typical"] <- "Low movement"
  result[distance_state == "high" & shuttle_state == "typical"] <- "High movement"
  result[distance_state == "typical" & shuttle_state == "low"] <- "Low shuttling"
  result[distance_state == "typical" & shuttle_state == "high"] <- "High shuttling"
  result
}

.classify_limits_activity <- function(limits,
                                      activity,
                                      limits_high,
                                      activity_low,
                                      activity_high,
                                      activity_name) {
  high_limits <- limits > limits_high
  low_activity <- activity <= activity_low
  high_activity <- activity >= activity_high

  result <- rep("Typical project range", length(limits))
  result[high_limits] <- "High limit exposure"
  result[low_activity] <- paste("Low", activity_name)
  result[high_activity] <- paste("High", activity_name)
  result[high_limits & low_activity] <- paste("High limit exposure + low", activity_name)
  result[high_limits & high_activity] <- paste("High limit exposure + high", activity_name)
  result
}
