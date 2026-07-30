#' ShuttleboxR: preparation and exploration of shuttle-box temperature data
#'
#' ShuttleboxR bridges the gap between data acquisition and statistical analysis
#' in shuttle-box temperature experiments. It imports and organises recordings,
#' calculates reproducible thermal and behavioural metrics, and provides plots
#' and project-level screens for inspecting data quality.
#'
#' @section Workflow:
#' The package follows three consecutive stages:
#'
#' 1. **Import and organise** the data.
#' 2. **Calculate shuttle-box metrics** using explicit settings.
#' 3. **Inspect and troubleshoot** the data before formal analysis.
#'
#' The stages are applied to two connected branches. The single-trial branch
#' calculates and inspects one fish. The project branch compiles one row of
#' metrics per fish, examines distributions and multivariate patterns across the
#' study, and flags trials that should be returned to the single-trial plots for
#' closer review.
#'
#' @section Interpretation:
#' Project-level outlier screens identify candidates for inspection, not
#' automatic exclusions. Unusual values should be checked against the original
#' temperature, tracking and movement records and interpreted in the context of
#' the species and experimental design. ShuttleboxR prepares and explores data;
#' it does not choose the inferential analysis for a study.
#'
#' @section Core temperature:
#' ShuttleSoft files normally already include `core_T`. Recalculation with
#' [calc_coreT()] is optional and requires calibrated thermal-lag parameters.
#'
#' @section Getting started:
#' Run `vignette("ShuttleboxR", package = "ShuttleboxR")` for a worked example
#' covering both the single-trial and project-level workflows.
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
