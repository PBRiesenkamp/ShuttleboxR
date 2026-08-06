# Perform principal component analysis on project data

Performs a scaled principal component analysis (PCA) of selected numeric
project-level shuttle-box metrics. PCA summarises correlated metrics as
new axes. Fish close together have similar multivariate profiles, while
separated fish differ in one or more metrics. Variable arrows show the
direction in which each metric increases and help identify which
measurements may be driving a flagged fish.

## Usage

``` r
pca(
  data,
  variables = NULL,
  mahalanobis_th = 0.99,
  dbscan_th = 1.5,
  dbscan_minPts = 4,
  flag_rule = c("both", "either", "mahalanobis", "dbscan"),
  print_labels = TRUE,
  id_col = "fileID",
  var_col = "Tpref",
  biplot_variables = TRUE,
  highlight_outliers = TRUE,
  n_driver_variables = 3
)
```

## Arguments

- data:

  Project-results data containing one row per trial or individual.

- variables:

  Character vector naming the numeric variables to include in the PCA.
  Supplying this argument is recommended because it makes the analysis
  explicit and prevents newly added numeric columns from silently
  changing the PCA. If `NULL`, all suitable numeric columns are used.

- mahalanobis_th:

  Probability used for the chi-squared Mahalanobis distance cutoff.
  Default is 0.99. Larger values are more conservative and flag fewer
  fish.

- dbscan_th:

  `eps` value supplied to DBSCAN. Default is 1.5. Larger values
  generally join more neighbouring fish and flag fewer as locally
  isolated.

- dbscan_minPts:

  Minimum number of neighbouring points required by DBSCAN. Default
  is 4. Larger values generally make the density screen more stringent,
  although the effect depends on sample size and structure.

- flag_rule:

  Rule used to define the fish highlighted as candidates for review. One
  of `"both"` (default), `"either"`, `"mahalanobis"`, or `"dbscan"`.
  `"both"` is the most conservative and highlights only fish identified
  by both screens.

- print_labels:

  Logical. Show labels for all individuals on PCA plots. Default is
  `TRUE`. Flagged fish are labelled when `highlight_outliers` is `TRUE`
  even when `print_labels = FALSE`.

- id_col:

  Identifier column. Default is `"fileID"`.

- var_col:

  Variable plotted against PC1. Default is `"Tpref"`.

- biplot_variables:

  Logical. Show variable vectors on the biplot.

- highlight_outliers:

  Logical. Circle and label fish selected by `flag_rule` on the biplot
  and PC1-variable plot. Default is `TRUE`.

- n_driver_variables:

  Number of unusually high or low original variables reported for each
  fish detected by at least one screen. Default is 3.

## Value

A list containing the PCA object, loadings, scores, variance explained,
method-level detections, a fish-level `screening` table, `flagged_ids`
selected by `flag_rule`, an `outlier_details` table describing potential
drivers, thresholds, retained identifiers, and plots.

## Details

Two complementary screens are provided. Mahalanobis distance identifies
fish far from the multivariate centre while accounting for covariance
among PCA dimensions. DBSCAN identifies fish in locally sparse regions
of the PC1-PC2 plot. `flag_rule` controls whether either screen, both
screens, or one named screen is used for highlighting. These screens
identify candidates for review, not automatic exclusions.

## Examples

``` r
if (FALSE) { # \dontrun{
project_data <- read_project_database(file.choose())
project_pca <- pca(
  project_data,
  variables = c(
    "Tpref", "Tpref_range", "grav_time", "tot_distance",
    "nr_shuttles", "t_near_max", "t_near_min"
  ),
  print_labels = FALSE
)
project_pca$plots$biplot
} # }
```
