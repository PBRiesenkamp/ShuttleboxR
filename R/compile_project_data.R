#' Compile all trials into a single datafile
#'
#' This function compiles all shuttle-box trials in a list object, as imported by read_shuttlesoft, in a single dataframe
#'
#' @param data_read A list object containing the imported shuttle-box trials
#' @return Dataframe of all shuttle-box trials in a shuttle-box project
#' @export

compile_project_data<-function(data_read){
  
  # Prepare the data using file_prepare. Multidat is TRUE because this will suppress warnings from preparing each data file separately
  data_list <- lapply(data_read, file_prepare)
  processed_list <- lapply(data_list, calc_coreT)
  combined_df <- do.call(rbind, processed_list)
  rownames(combined_df)<-NULL
  return(combined_df)
}