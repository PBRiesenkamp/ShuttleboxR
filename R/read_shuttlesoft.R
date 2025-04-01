#' Import shuttlesoft shuttle-box data
#'
#' This function imports .txt files output by shuttlesoft
#'
#' @param file A string containing the directory of the file to be loaded.
#' @param metadata A metadata file containing the variables file_name, initial_temp, mass, a_value and b_value
#' @param multidat A parameter that specifies whether this considers a single trial or multiple trials, default is FALSE
#' @return a raw shuttle-box dataframe
#' @export

read_shuttlesoft <- function (file, metadata, multidat = F){
  # Load .txt data file that shuttlesoft produces into R
  # Don't load headers, because the additional info at the top of the .txt file will make a confusing dataframe
  # Rename data columns
  data<-read.delim(file,
                   header = F,
                   col.names = (c("time", "zone", "core_T", "Tpref_loligo", "INCR_T",
                                  "DECR_T", "x_pos", "y_pos", "velocity", "distance", "time_in_INCR", "time_in_DECR",
                                  "delta_T", "dyn_hysteresis", "stat_T_INCR", "stat_hyst_INCR", "stat_T_DECR",
                                  "stat_hyst_DECR", "k", "max_T", "min_T", "change_rate", "avoidance_upper",
                                  "avoidance_upper_core", "avoidance_lower", "avoidance_lower_core")),
                   
                   na.strings = c("NaN", ""))
  
  # Extract notes, the file created info and the pixel ratio from the dataframe
  data<-data[!is.na(data$time), ]
  if (length(data$zone[data$time == "Notes"]) == 0 || is.na(data$zone[data$time == "Notes"])) {
    notes <- NA
  }else{
    notes <- data$zone[data$time == "Notes"]
  }
  filecreated <- as.POSIXct(data$zone[data$time == "File created"], format = "%d/%m/%Y; %H:%M")
  pixel_ratio <- data$zone[data$time == "Pixel Ratio [cm/pix]"]
  
  # Remove the additional info and reset rownames
  data<-data[!is.na(data$INCR_T), ]
  numeric_rows <- !is.na(suppressWarnings(as.numeric(data$INCR_T)))
  data<-data[numeric_rows, ]
  rownames(data)<-NULL 
  
  # Add notes and pixel ratio
  data$notes <- notes
  data$pixel_ratio <- pixel_ratio
  
  # Add the date of the trial by extracting it from the filecreated object
  data$date <- as.Date(filecreated)
  fileID <- basename(file)
  data$fileID <- fileID
  
  trialstart <-  function(metadata){
    
    if (missing(metadata)) {
      trial_start <- data$time[1]
      warning("Metadata not provided; cannot define 'trial_start'")
    } else {
      
      # If the metadata is present, ensure necessary columns exist in the metadata
      if (!all(c("file_name", "trial_start", "mass", "a_value", "b_value") %in% colnames(metadata))) {
        warning("The metadata does not contain one or more valuable columns: 'file_name', 'trial_start', 'mass', 'a_value', 'b_value'.")
      }
      
      # Find rows with NA in specified columns
      columns_to_check <- intersect(c("file_name", "trial_start", "mass", "a_value", "b_value"), colnames(metadata))
      rows_with_na <- apply(metadata[columns_to_check], 1, function(row) any(is.na(row)))
      
      # If there are any rows with NA, throw a warning mentioning file names
      if (any(rows_with_na)) {
        names_with_na <- metadata$file_name[rows_with_na]
        warning (paste("The metadata contains NA values in one or more of these columns: 'file_name', 'trial_start', 'mass', 'a_value', 'b_value'
",paste(names_with_na, collapse = "\n ")))
      }
      
      #Ensure necessary columns are in the right format
      if (all(grepl("^\\d{2}:\\d{2}:\\d{2}$", metadata$trial_start))==F){
        message("Warning: One or more entries in trial_start are not in an hh:mm:ss format")
      }
      
      if(!("trial_start" %in% colnames(metadata))){
        trial_start <- data$time[1]
      }else if("trial_start" %in% colnames(metadata) && is.na(metadata$trial_start[metadata$file_name==fileID])){
        trial_start <- data$time[1]
      }else {
        trial_start<-metadata$trial_start[metadata$file_name==fileID]
      }
    }
    return(trial_start)}
  
  if (multidat) {
    data$trial_start <- suppressWarnings(trialstart (metadata = metadata))} else {
      data$trial_start <- trialstart(metadata = metadata)}
  
  data$trial_start <- as.character(data$trial_start)
  
  data$a_value <- NA
  data$b_value <- NA
  data$mass <- NA
  data$initial_T <- NA
  data$a_value <- metadata$a_value[metadata$file_name == fileID]
  data$b_value <- metadata$b_value[metadata$file_name == fileID]
  data$mass <- metadata$mass[metadata$file_name == fileID]
  data$initial_T <- metadata$initial_T[metadata$file_name == fileID] 
  
  return(data)
}