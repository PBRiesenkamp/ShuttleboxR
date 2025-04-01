#' Import all shuttlesoft files in a directory
#'
#' This function imports all the .txt shuttlesoft files in a directory according to read_shuttlesoft and compiles them into a list object
#'
#' @param metadata A metadata file containing the variables file_name, initial_temp, mass, a_value and b_value for each trial in the directory
#' @param directory The directory the function will search in for the .txt files, default is the working directory
#' @return List object containing imported raw shuttlesoft data
#' @export

read_shuttlesoft_project <- function (metadata, directory = getwd()){
  
  # Check if metadata is specified and give warnings for what happens if data is missing
  if (missing(metadata)) {
    warning("Metadata not provided; cannot define one or more of the following variables: 'trial_start', 'a-value', 'b-value', 'initial_temp.'")
  } else {
    
    # If the metadata is present, ensure necessary columns exist in the metadata
    if (!all(c("file_name", "trial_start", "mass", "a_value", "b_value") %in% colnames(metadata))) {
      warning("The metadata does not contain one or more necessary columns: 'file_name', 'trial_start', 'mass', 'a_value', 'b_value'.")
    }
    
    # Find rows with NA in specified columns
    columns_to_check <- intersect(c("file_name", "trial_start", "mass", "a_value", "b_value"), colnames(metadata))
    rows_with_na <- apply(metadata[columns_to_check], 1, function(row) any(is.na(row)))
    
    # If there are any rows with NA, throw warning mentioning file names
    if (any(rows_with_na)) {
      names_with_na <- metadata$file_name[rows_with_na]
      warning (paste("The metadata contains NA values in one or more of these columns: 'file_name', 'trial_start', 'mass', 'a_value', 'b_value'
",paste(names_with_na, collapse = "\n ")))
    }
    
    #Ensure necessary columns are in the right format
    if (all(grepl("^\\d{2}:\\d{2}:\\d{2}$", metadata$trial_start))==F){
      message("Warning: One or more entries in trial_start are not in an hh:mm:ss format")
    }
  }
  
  # Search for all the .txt files in the specified directory
  txt_files <- list.files(path = directory, pattern = "\\.txt$", full.names = TRUE)
  
  # Check if all files are represented in the metadata
  file_names <- basename(txt_files)
  missing_files <- setdiff(file_names, metadata$file_name)
  if (length(missing_files) > 0) {
    stop(paste("The following files are not represented in metadata:", 
               paste(missing_files, collapse = "\n ")))
  }
  
  # Read those text files and compile them into a list
  data_read <- lapply(txt_files, read_shuttlesoft, metadata = metadata, multidat = T)
  
  # Name each list element as the directory of the text files
  names(data_read)<-txt_files
  
  return(data_read)
}
