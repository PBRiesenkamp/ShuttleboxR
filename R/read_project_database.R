#' Read a ShuttleboxR project-results database
#'
#' Imports a comma-separated project-results table and standardises common
#' column names used by older ShuttleboxR versions. This makes existing project
#' databases compatible with the current project-level plotting and analysis
#' functions.
#'
#' @param file Path to a project-results `.csv` file.
#' @param standardise_names Logical. If `TRUE` (the default), legacy names such
#'   as `study_ID`, `distance`, and `shuttles` are converted to the current names
#'   `fileID`, `tot_distance`, and `nr_shuttles`.
#'
#' @return A data frame containing one row per trial or individual.
#'
#' @details
#' The following legacy names are recognised: `study_ID`, `distance`,
#' `shuttles`, `pref_range`, `time_near_max`, `time_near_min`, and
#' `time_near_limits`. The original values are retained; only the column names
#' are changed. If no identifier column is available but `ID` is present, a
#' character `fileID` column is added.
#'
#' Project databases store summary values and therefore cannot be used to
#' calculate a new `Tbreadth` or `Tpercentile_range` value retrospectively.
#' These metrics must already be present in the table or be calculated from the
#' underlying temperature observations using [calc_project_results()].
#'
#' @examples
#' \dontrun{
#' project_data <- read_project_database(file.choose())
#' plot_distance_vs_shuttles(project_data, highlight_cases = TRUE)
#' }
#'
#' @seealso [calc_project_results()], [plot_histograms()], [pca()]
#' @export
read_project_database <- function(file, standardise_names = TRUE) {
  if (length(file) != 1L || is.na(file) || !file.exists(file)) {
    stop("`file` must be the path to an existing CSV file.", call. = FALSE)
  }

  data <- utils::read.csv(
    file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (isTRUE(standardise_names)) {
    data <- .standardise_project_data(data)
  }

  data
}
