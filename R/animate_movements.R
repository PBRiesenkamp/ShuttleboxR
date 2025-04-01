#' Animate the movement of the subject during the trial
#'
#' This function animates the movement of the subject during the trial
#'
#' @param data An organised shuttle-box dataframe with corrected core body temperature
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @param speed Speed setting of animation, default is 100
#' @import rgl 
#' @return Animation of the subject during the trial
#' @export


animate_movements <- function(data, exclude_start_minutes = 0, exclude_end_minutes = 0, speed = 100) {
  # Ensure necessary columns exist
  if (!all(c("x_pos", "y_pos", "time_sec", "core_T") %in% colnames(data))) {
    stop("The dataset does not contain one or more necessary columns: 'x_pos', 'y_pos', 'time_sec', 'core_T'.")
  }
  
  # Convert time_sec to numeric if not already
  data$time_sec <- as.numeric(data$time_sec)
  
  # Exclude rows based on the exclude_start_minutes and exclude_end_minutes parameters
  start_time <- exclude_start_minutes * 60
  end_time <- max(data$time_sec, na.rm = TRUE) - (exclude_end_minutes * 60)
  data <- data[data$time_sec >= start_time & data$time_sec <= end_time, ]
  
  # Set up 3D plot
  rgl::plot3d(data$x_pos, data$y_pos, data$time_sec, type = "l", col = "darkgrey", alpha = 0.15,
              xlab = "X Position", ylab = "Y Position", zlab = "Time (seconds)")
  
  # Determine color scaling based on Tpref
  colors <- c("dodgerblue", "brown1")(length(unique(data$core_T)))
  data$color <- colors[as.numeric(cut(data$core_T, breaks = length(colors)))]
  
  # # Animate the plot
  # for (i in seq(1, nrow(data))) {  # Update every 10 points
  #   rgl::points3d(data$x_pos[i], data$y_pos[i], data$time_sec[i], col = data$color[i], size = 5)
  # }
  # 
  # rgl::play3d(spin3d(axis = c(0, 0, 1), rpm = 100), duration = 5)
  
  run_speed = 10^speed^speed
  
  # Function to update the animation
  animate_step <- function(time) {
    index <- floor(time * run_speed) + 1  # Adjust speed
    if (index > nrow(data)) return()
    rgl::points3d(data$x_pos[index], data$y_pos[index], data$time_sec[index], col = data$color[index], size = 5)
  }
  
  # Use play3d with a time sequence
  rgl::play3d({
    for (i in seq_len(nrow(data))) {
      rgl::points3d(data$x_pos[i], data$y_pos[i], data$time_sec[i], col = data$color[i], size = 5)
      Sys.sleep(1 / (run_speed * 100))  # Adjust speed dynamically
    }
  }, duration = nrow(data) / (run_speed * 10))
}
