#' ShuttleboxR: analysis of shuttle-box temperature experiments
#'
#' ShuttleboxR imports, prepares, checks, analyses, and visualises data produced
#' by ShuttleSoft shuttle-box experiments. It supports both single-trial and
#' multi-file workflows.
#'
#' @section Typical workflow:
#' 1. Import one file with [read_shuttlesoft()] or a folder of files with
#'    [read_shuttlesoft_project()].
#' 2. Inspect the imported data with [inspect()].
#' 3. Calculate thermal and behavioural metrics such as [calc_Tpref()],
#'    [calc_Tavoid()], [calc_Tbreadth()], [calc_shuttles()], and
#'    [calc_occupancy()].
#' 4. Visualise individual trials with functions such as
#'    [plot_T_segmented()], [plot_coreT_histogram()], [plot_tracking()], and
#'    [plot_heatmap()].
#' 5. Summarise a complete project with [calc_project_results()].
#'
#' @section Core temperature:
#' ShuttleSoft files normally already include `core_T`. Recalculation with
#' [calc_coreT()] is optional and requires calibrated thermal-lag parameters.
#'
#' @section Getting started:
#' Run `vignette("ShuttleboxR", package = "ShuttleboxR")` for a complete worked
#' example using the file included with the package.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom graphics hist
#' @importFrom graphics pairs
#' @importFrom graphics panel.smooth
#' @importFrom graphics text
#' @importFrom stats aggregate
#' @importFrom stats cor
#' @importFrom stats cov
#' @importFrom stats density
#' @importFrom stats dexp
#' @importFrom stats dnorm
#' @importFrom stats lm
#' @importFrom stats mahalanobis
#' @importFrom stats median
#' @importFrom stats na.omit
#' @importFrom stats predict
#' @importFrom stats qchisq
#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom utils read.delim
#' @importFrom utils type.convert
## usethis namespace: end
NULL
