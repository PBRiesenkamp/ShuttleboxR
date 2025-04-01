#' Calculate the core body temperature
#'
#' This function calculates the core body temperature changes during the trial
#'
#' @param data an organised shuttle-box data file, as output by file_prepare
#' @return a data frame with corrected core body temperature values
#' @export

calc_coreT <- function(data) {
  
  # Initialize the core_T column
  if ("initial_T" %in% colnames(data) && any(!is.na(data$initial_T))) {
    data$core_T <- NA
    data$core_T[1] <- unique(data$initial_T)} else {
      if(!"core_T" %in% colnames(data) || all(is.na(data[["core_T"]])))
        stop("Initial temperature not defined, cannot calculate core body temperature")
    }
  
  # Ensure necessary columns exist
  if (!any(c("a_value", "b_value", "mass", "shuttle") %in% colnames(data))) {
    stop("The dataset does not contain one or more necessary columns: 'a_value', 'b_value', 'mass','shuttle'.")
  }
  
  #Ensure necessary columns are in the right format
  if (!all(is.numeric(c(data$a_value, data$b_value, data$mass, data$core_T, data$shuttle)))){
    stop("One or more necessary variables are not in the right format")
  }
  
  if (any(c(length(unique(data$a_value)),length(unique(data$b_value)), length(unique(data$mass)))>1)){
    stop("One or more of the following parameters have more than 1 value: 'a_value', 'b_value', 'mass' ")
  }
  
  a_value <- unique(data$a_value)
  b_value <- unique(data$b_value)
  mass <- unique(data$mass)
  
  k <- a_value * mass^b_value
  
  data$ambient_T<-ifelse(data$zone == "INCR", data$INCR_T, data$DECR_T)
  
  # Calculate body core temperature from second to second
  for (i in 2:nrow(data)) {
    if (data$shuttle[i] == 1) {
      data$core_T[i] <- data$core_T[i-1]  # Carry forward the body core temperature at the moment of shuttling
    } else {
      data$core_T[i] <- data$ambient_T[i] + 
        (data$core_T[i-1] - data$ambient_T[i]) * exp(-k * 1 / 60)
    }
  }
  
  return(data)
}