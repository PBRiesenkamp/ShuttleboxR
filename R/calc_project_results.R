#' Calculate shuttle-box metrics for all shuttle-box trials
#'
#' This function calculates shuttle-box metrics for all trials in a list object, as imported by read_shuttlesoft
#'
#' @param data_read A list object containing the imported shuttle-box trials
#' @param calculate_distance Boolean to determine whether to calculate the distance according to calculate_distance, default is FALSE
#' @param pixel_to_cm Specify whether to convert coordinate units in pixels to coordinate units in cm if calculate_distance is TRUE, default is TRUE
#' @param exclude_acclimation Exclude the acclimation period from variable calculation, default = F
#' @param exclude_start_minutes Exclusion of time from the start of the trial onwards, in minutes. Default is 0
#' @param exclude_end_minutes Exclusion of time from the end of the trial backwards, in minutes. Default is 0
#' @param Tpref_method  The method used for calculation of temperature preference ("median", "mean", "mode"). Default is "median"
#' @param Tavoid_percentiles The lower and upper percentile for lower and upper avoidance temperature calculation resp. Default is c(0.05, 0.95)
#' @param textremes_threshold Definition of the extreme temperature range. Default is 20% of the temperature range: expression(0.2*(max(data$max_T)-max(data$min_T)))
#' @param core_T_variance_type The method used for calculating the variance of core body temperature ("std_error", "std_deviation", "coeff_variation"). Default is "std_error".
#' @return Dataframe containing shuttle-box metrics for all trials in the project
#' @export


calc_project_results <- function(data_read,
                                 calculate_distance = F,
                                 pixel_to_cm = T,
                                 exclude_acclimation = F,
                                 exclude_start_minutes = 0, 
                                 exclude_end_minutes = 0,
                                 Tpref_method = "median",
                                 Tavoid_percentiles = c(0.05, 0.95),
                                 textremes_threshold = expression(0.2 * (max(df$max_T) - max(df$min_T))),
                                 core_T_variance_type = "std_error"){
  message("Initialising...")
  
  # Prepare the data using file_prepare. Multidat is TRUE because this will suppress warnings from preparing each data file separately
  data_list <- lapply(data_read, file_prepare)
  
  
  # Create a function that pulls all the information from a datafile in one go, using the functions specified earlier 
  apply_functions <- function(df, df_name) {
    
    # Define the fileID the function is currently considering
    fileID <- unique(df$fileID)
    
    # Calculate core body temperature
    df<-calc_coreT(df)
    
    # Calculate distance covered if true
    if (calculate_distance) df<-calc_distance (df, pixel_to_cm)
    
    # Exclude acclimation period if requested from the start. Acclimation period is defined in file_prepare
    if(exclude_acclimation) {
      df <- subset(df, df$trial_phase != "acclimation") 
    }
    
    # Subset custom time window
    df <- df[df$time_sec >= exclude_start_minutes * 60 & df$time_sec <= max(df$time_sec) - (exclude_end_minutes * 60), ]
    
    
    textremes_th <- eval(textremes_threshold) 
    
    # Calculate all variables using default settings (these can be edited in the main function)
    Tpref <- calc_Tpref(df, method = Tpref_method, print_results = F)
    grav_time <- calc_gravitation(df, print_results = F)
    tot_distance <- calc_tot_distance(df, print_results = F)
    Tavoid <- calc_Tavoid(df, percentiles = Tavoid_percentiles, print_results = F)
    Tpref_range <- Tavoid[2] - Tavoid[1]
    textremes <- calc_extremes(df, threshold = textremes_th, print_results = F)
    textremes_tot <- textremes[1]+textremes[2]
    track_accuracy <- calc_track_accuracy(df, print_results = F)
    core_T_variance <- calc_coreT_variance(df, variance_type = core_T_variance_type)
    nr_shuttles <- calc_shuttles(df, print_results = F)
    chamber_seconds <- calc_occupancy(df, print_results = F)
    
    # Return a list of the variables you want
    return(list(fileID = fileID,
                Tpref = Tpref,
                Tpref_range = Tpref_range,
                grav_time = grav_time, 
                tot_distance = tot_distance,
                Tavoid = Tavoid,
                textremes = textremes,
                textremes_tot = textremes_tot,
                track_accuracy = track_accuracy,
                core_T_variance = core_T_variance,
                nr_shuttles = nr_shuttles,
                chamber_seconds = chamber_seconds))
  }
  
  # Apply functions to each dataframe in the list and collect results
  results <- do.call(rbind, lapply(names(data_list), function(name, total) {
    
    current_iter <- which(names(data_list) == name)  
    message("Processing dataset ", current_iter, " of ", total, " (", round((current_iter / total) * 100, 2), "% complete)")  
    
    # Apply the functions
    func_results <- apply_functions(df = data_list[[name]], df_name = name)
    
    # Combine results into a data frame
    data.frame(
      fileID = func_results$fileID,
      Tpref = func_results$Tpref,
      Tpref_range = func_results$Tpref_range,
      grav_time  = func_results$grav_time,
      tot_distance  = func_results$tot_distance,
      Tavoid_lower  = func_results$Tavoid[1],
      Tavoid_upper = func_results$Tavoid[2],
      t_near_min  = func_results$textremes[1],
      t_near_max  = func_results$textremes[2],
      t_near_limits = func_results$textremes_tot,
      track_accuracy = func_results$track_accuracy,
      core_T_variance  = func_results$core_T_variance,
      nr_shuttles  = func_results$nr_shuttles,
      seconds_in_DECR = func_results$chamber_seconds[1],
      seconds_in_INCR = func_results$chamber_seconds[2]
      
    )
    
  }, total = length(data_list)))
  
  # Reset rownames as they have acquired names from the list
  rownames(results) <- NULL
  
  return(results)
}