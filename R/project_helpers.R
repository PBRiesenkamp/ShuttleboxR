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
