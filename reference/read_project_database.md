# Read a ShuttleboxR project-results database

Imports a comma-separated project-results table and standardises common
column names used by older ShuttleboxR versions. This makes existing
project databases compatible with the current project-level plotting and
analysis functions.

## Usage

``` r
read_project_database(file, standardise_names = TRUE)
```

## Arguments

- file:

  Path to a project-results `.csv` file.

- standardise_names:

  Logical. If `TRUE` (the default), legacy names such as `study_ID`,
  `distance`, and `shuttles` are converted to the current names
  `fileID`, `tot_distance`, and `nr_shuttles`.

## Value

A data frame containing one row per trial or individual.

## Details

The following legacy names are recognised: `study_ID`, `distance`,
`shuttles`, `pref_range`, `time_near_max`, `time_near_min`, and
`time_near_limits`. The original values are retained; only the column
names are changed. If no identifier column is available but `ID` is
present, a character `fileID` column is added.

Project databases store summary values and therefore cannot be used to
calculate a new `Tbreadth` value retrospectively. Thermal breadth must
be calculated from the underlying temperature observations using
[`calc_project_results()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_project_results.md).

## See also

[`calc_project_results()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_project_results.md),
[`plot_histograms()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_histograms.md),
[`pca()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/pca.md)

## Examples

``` r
example_file <- system.file(
  "extdata",
  "project_database_example.csv",
  package = "ShuttleboxR"
)

project_data <- read_project_database(example_file)
head(project_data)
#>   ID total_length mass grav_time tot_distance nr_shuttles    Tpref Tavoid_lower
#> 1  1         14.0   30  3.935278     13358.74    96.66285 19.84438     18.47219
#> 2  2         12.7   28  3.963889     12599.95   114.75720 20.05440     18.48400
#> 3  3         12.5   22  3.617222     22308.84    93.16992 20.06635     18.73885
#> 4  4         12.9   27  4.788333     19816.90    82.61885 18.53578     17.21512
#> 5  5         11.2   16  3.937778     21400.48   122.65630 20.36958     19.20577
#> 6  6         13.9   29  3.460833     16928.99    43.08452 16.87928     15.21686
#>   Tavoid_upper Tpref_range t_near_max t_near_min t_near_limits          fileID
#> 1     21.51655    3.044359          0          0             0     Fish_1_13_3
#> 2     21.21841    2.734412          0          0             0     Fish_2_13_2
#> 3     20.88207    2.143220          0          0             0 Fish_3_20_3_rev
#> 4     19.60535    2.390230          0          0             0 Fish_4_20_3_rev
#> 5     21.34257    2.136799          0          0             0 Fish_5_20_2_rev
#> 6     19.00942    3.792558          0          0             0 Fish_6_13_3_rev
```
