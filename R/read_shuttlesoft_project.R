#' Import all ShuttleSoft files in a directory
#'
#' Imports every ShuttleSoft `.txt` and `.csv` file in a directory and returns
#' them as a named list. A metadata table is optional. When supplied, it may
#' provide different trial start times or thermal-model values for each file.
#'
#' @param metadata Optional data frame containing `file_name` and any of
#'   `trial_start`, `mass`, `initial_T`, `a_value`, or `b_value`.
#' @param directory Directory containing the ShuttleSoft files. The default is
#'   the current working directory.
#' @param prepare Logical. If `TRUE` (the default), prepare each file with
#'   [file_prepare()].
#'
#' @return A named list of imported ShuttleSoft data frames.
#'
#' @details
#' Files are imported in alphabetical order. If `metadata` is supplied, values
#' are matched to each file using the `file_name` column.
#'
#' @seealso [read_shuttlesoft()], [calc_project_results()]
#' @export
read_shuttlesoft_project <- function(metadata = NULL,
                                     directory = getwd(),
                                     prepare = TRUE) {

  if (length(directory) != 1L || !dir.exists(directory)) {
    stop("`directory` must be an existing folder.", call. = FALSE)
  }

  shuttle_files <- list.files(
    path = directory,
    pattern = "\\.(txt|csv)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(shuttle_files) == 0L) {
    stop("No `.txt` or `.csv` files were found in the selected directory.", call. = FALSE)
  }

  if (!is.null(metadata)) {
    if (!is.data.frame(metadata) || !"file_name" %in% names(metadata)) {
      stop("`metadata` must be a data frame containing `file_name`.", call. = FALSE)
    }

    missing_files <- setdiff(basename(shuttle_files), as.character(metadata$file_name))
    if (length(missing_files) > 0L) {
      warning(
        "No metadata row was found for: ",
        paste(missing_files, collapse = ", "),
        ". These files will still be imported.",
        call. = FALSE
      )
    }
  }

  data_read <- lapply(
    shuttle_files,
    function(path) {
      read_shuttlesoft(
        file = path,
        metadata = metadata,
        prepare = prepare,
        multidat = TRUE
      )
    }
  )

  names(data_read) <- basename(shuttle_files)
  data_read
}
