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
calculate a new `Tbreadth` or `Tpercentile_range` value retrospectively.
These metrics must already be present in the table or be calculated from
the underlying temperature observations using
[`calc_project_results()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_project_results.md).

## See also

[`calc_project_results()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_project_results.md),
[`plot_histograms()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_histograms.md),
[`pca()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/pca.md)

## Examples

``` r
if (FALSE) { # \dontrun{
project_data <- read_project_database(file.choose())
plot_distance_vs_shuttles(project_data, highlight_cases = TRUE)
} # }
```
