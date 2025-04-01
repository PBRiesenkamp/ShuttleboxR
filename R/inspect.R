#' Inspect imported shuttle-box data
#'
#' This function inspects shuttle-box data to check whether all variables are present and formatted correctly
#'
#' @param data an imported shuttle-box data file
#' @return NULL
#' @export

inspect <- function(data){
  
  if(!all(c("date", "time", "zone", "INCR_T", "DECR_T") %in% colnames(data))){
    stop("Dataset is missing one or more of the following necessary columns:  'date', 'time', 'zone', 'INCR_T', 'DECR_T'")
  }
  else if (!all(c("x_pos", "y_pos", "distance") %in% colnames(data))) {
    warning("Dataset is missing 'x_pos', 'y_pos', 'distance', no way to determine distance found")
  }
  else if(!all(c( "velocity", "time_in_INCR", "time_in_DECR",
                  "delta_T", "dyn_hysteresis", "stat_T_INCR", "stat_hyst_INCR", "stat_T_DECR",
                  "stat_hyst_DECR", "max_T", "min_T", "change_rate")%in% colnames(data))) {
    warning("Dataset is missing one or more of the following columns:'velocity', 'time_in_INCR', 'time_in_DECR', 'delta_T', 'dyn_hysteresis', 'stat_T_INCR', 'stat_hyst_INCR', 'stat_T_DECR', 'stat_hyst_DECR', 'max_T', 'min_T', 'change_rate'")
  } else {print("No errors were found in the dataset")}
}