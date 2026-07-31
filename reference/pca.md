# Perform principal component analysis on project data

Performs a scaled principal component analysis of numeric project-level
shuttle-box metrics. Fish close together have similar multivariate
profiles, while separated fish differ in one or more metrics. Variable
arrows show the direction in which each metric increases and help
identify measurements that may be driving a flagged fish.

## Usage

``` r
pca(
  data,
  mahalanobis_th = 0.975,
  dbscan_th = 1,
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

- mahalanobis_th:

  Probability used for the chi-squared Mahalanobis distance cutoff.
  Default is 0.975. Smaller values flag more fish.

- dbscan_th:

  `eps` value supplied to DBSCAN. Default is 1. Smaller values generally
  classify more fish as locally isolated.

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

  Logical. Circle and label fish flagged by either outlier screen on the
  biplot and PC1-variable plot. Default is `TRUE`.

- n_driver_variables:

  Number of unusually high or low original variables reported for each
  flagged fish. Default is 3.

## Value

A list containing the PCA object, loadings, scores, method-level outlier
table, an `outlier_details` table describing potential drivers, retained
row identifiers, variance explained, and plots.

## Details

Mahalanobis distance identifies fish far from the multivariate centre
while accounting for covariance among PCA dimensions. DBSCAN identifies
fish in locally sparse regions of the PC1-PC2 plot. These screens
identify candidates for review, not automatic exclusions.

## Examples

``` r
example_file <- system.file(
  "extdata", "project_database_example.csv", package = "ShuttleboxR"
)
project_data <- read_project_database(example_file)

pca_data <- project_data[c(
  "fileID", "Tpref", "Tpref_range", "grav_time",
  "tot_distance", "nr_shuttles", "t_near_max", "t_near_min"
)]

pca_result <- pca(pca_data, print_labels = FALSE)
#> Warning: This FactoMineR PCA result contains only 5 eigenvalues and does not include the complete spectrum. Refit the PCA with a larger `ncp` before drawing a complete scree plot.
pca_result$plots$biplot

pca_result$outlier_details
#>                 fileID              methods mahalanobis_distance
#> 1          Fish_8_13_2 Mahalanobis + DBSCAN            23.408263
#> 2     Fish_28_13_3_rev Mahalanobis + DBSCAN            23.427783
#> 3       Fish_48_13_2_S Mahalanobis + DBSCAN            30.773541
#> 4       Fish_49_13_3_B          Mahalanobis            15.223855
#> 5       Fish_52_20_2_S Mahalanobis + DBSCAN            15.145753
#> 6   Fish_55_13_2_rev_S Mahalanobis + DBSCAN            16.719296
#> 7   Fish_57_20_2_rev_S Mahalanobis + DBSCAN            21.165685
#> 8   Fish_59_13_2_rev_S Mahalanobis + DBSCAN            20.633282
#> 9  Fish_76_20_2_B_long          Mahalanobis            14.389787
#> 10        Fish_13_13_3               DBSCAN             9.144939
#> 11    Fish_17_20_2_rev               DBSCAN            11.706750
#> 12        Fish_33_20_3               DBSCAN             6.350096
#> 13      Fish_46_20_2_S               DBSCAN             6.601730
#> 14      Fish_53_13_2_S               DBSCAN            10.876938
#> 15  Fish_63_20_2_rev_S               DBSCAN             9.330059
#> 16  Fish_81_20_2_rev_B               DBSCAN             5.636258
#>                                                                   potential_drivers
#> 1       t_near_max high (4.2 SD); grav_time low (-3 SD); tot_distance high (2.4 SD)
#> 2  nr_shuttles high (4.9 SD); tot_distance high (1.4 SD); Tpref_range low (-1.1 SD)
#> 3            t_near_min high (5.2 SD); grav_time high (3.7 SD); Tpref low (-3.2 SD)
#> 4    grav_time high (3.7 SD); Tpref_range low (-0.7 SD); tot_distance low (-0.4 SD)
#> 5       t_near_min high (3.1 SD); Tpref_range high (3 SD); t_near_max high (2.1 SD)
#> 6          t_near_min high (4.2 SD); Tpref_range high (2.7 SD); Tpref low (-2.4 SD)
#> 7            t_near_max high (4.7 SD); Tpref high (2.2 SD); grav_time low (-1.2 SD)
#> 8          t_near_max high (4.3 SD); Tpref high (2.2 SD); Tpref_range high (1.9 SD)
#> 9      grav_time high (3.2 SD); Tpref_range high (1.4 SD); tot_distance low (-1 SD)
#> 10         Tpref low (-3.4 SD); t_near_min high (2.2 SD); nr_shuttles low (-1.2 SD)
#> 11  t_near_min high (3.3 SD); Tpref_range high (2.4 SD); tot_distance high (1.6 SD)
#> 12         t_near_max high (2.3 SD); Tpref high (1.7 SD); nr_shuttles low (-0.5 SD)
#> 13  Tpref_range high (1.7 SD); tot_distance high (1.5 SD); t_near_max high (1.2 SD)
#> 14         Tpref low (-3.1 SD); t_near_min high (2.6 SD); Tpref_range high (1.2 SD)
#> 15  t_near_max high (2.6 SD); Tpref_range high (2.3 SD); tot_distance low (-1.5 SD)
#> 16         Tpref_range high (2.1 SD); tot_distance low (-2 SD); Tpref low (-1.4 SD)
```
