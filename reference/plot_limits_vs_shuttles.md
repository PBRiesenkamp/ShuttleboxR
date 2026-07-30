# Plot time spent near limits versus shuttles across project data

Plots the number of shuttles against time spent close to the programmed
temperature limits for each trial or individual.

## Usage

``` r
plot_limits_vs_shuttles(proj_data, id_col = "fileID", label_points = TRUE)
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
