# Perform principal component analysis on project data

Performs a scaled principal component analysis (PCA) of numeric
project-level shuttle-box metrics. The returned object includes scores,
loadings, two complementary outlier screens, and four ready-to-display
plots.

## Usage

``` r
pca(
  data,
  mahalanobis_th = 0.7,
  dbscan_th = 1,
  print_labels = TRUE,
  id_col = "fileID",
  var_col = "Tpref",
  biplot_variables = TRUE
)
```

## Arguments

- data:

  Project-results data containing one row per trial or individual.

- mahalanobis_th:

  Probability used for the chi-squared Mahalanobis distance cutoff.
  Default is 0.7, retained for compatibility with earlier ShuttleboxR
  versions.

- dbscan_th:

  `eps` value supplied to DBSCAN. Default is 1.

- print_labels:

  Logical. Show individual labels on PCA plots. Default is `TRUE`.

- id_col:

  Identifier column. Default is `"fileID"`.

- var_col:

  Variable plotted against PC1. Default is `"Tpref"`.

- biplot_variables:

  Logical. Show variable vectors on the biplot.

## Value

A list containing the PCA object, loadings, scores, outlier table,
retained row identifiers, and plots.

## Examples

``` r
example_file <- system.file(
  "extdata", "project_database_example.csv", package = "ShuttleboxR"
)
project_data <- read_project_database(example_file)

pca_data <- project_data[c(
  "fileID", "mass", "Tpref", "Tavoid_lower", "Tavoid_upper",
  "Tpref_range", "tot_distance", "nr_shuttles"
)]

pca_result <- pca(pca_data, print_labels = FALSE)
#> Warning: This FactoMineR PCA result contains only 5 eigenvalues and does not include the complete spectrum. Refit the PCA with a larger `ncp` before drawing a complete scree plot.
pca_result$plots$biplot
```
