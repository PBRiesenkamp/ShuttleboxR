#' Calculate the cumulative distance
#'
#' This function calculates the distance covered for each observation during the trial.
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param pixel_to_cm if TRUE, converts coordinates with pixels as unit to coordinates with cm as unit. Default is TRUE
#' @return A shuttle-box dataframe with distance calculated for each observation
#' @export

calc_distance <- function(data, pixel_to_cm = TRUE){
  
  # If coordinates are missing, make them the last known coordinate
  for (i in 2:nrow(data)) {
    if(is.na(data$x_pos[i])) data$x_pos[i]<-data$x_pos[i-1]
    if(is.na(data$y_pos[i])) data$y_pos[i]<-data$y_pos[i-1]
  }
  
  #Initialise the velocity column
  data$velocity<-NA
  
  # Calculate the distance covered in each observation (velocity) using Pythagoras
  for (i in 2:nrow(data)) {
    data$velocity[i] <- sqrt((data$x_pos[i] - data$x_pos[i-1])^2 + (data$y_pos[i]-data$y_pos[i-1])^2)
  }
  
  # Change any NA velocities to zero
  data$velocity[is.na(data$velocity)]<-0
  
  # If the coordinates are specified in pixels rather than cm, convert
  if (pixel_to_cm){
    data$velocity <- data$pixel_ratio*data$velocity
  }
  
  #Initialise the distance column
  data$distance <- 0
  
  # Calculate cumulative distance
  for (i in 2:nrow(data)){
    data$distance[i]<- data$distance[i-1]+data$velocity[i-1]
  }
  
  return(data)
  
}