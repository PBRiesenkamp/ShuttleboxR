# Plot distance versus shuttles across project data

Plots total movement distance against the number of shuttles for each
trial or individual in a project-results table.

## Usage

``` r
plot_distance_vs_shuttles(proj_data, id_col = "fileID", label_points = TRUE)
```

## Arguments

- proj_data:

  Project-results data frame.

- id_col:

  Identifier column used for labels. Default is `"fileID"`.

- label_points:

  Logical. Label individual points. Default is `TRUE`.

## Value

A `ggplot2` plot, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
project_data <- read_project_database(file.choose())
plot_distance_vs_shuttles(project_data, highlight_cases = TRUE)
} # }
```
