# Plot distance versus time spent near limits across project data

Plots total movement distance against time spent close to the programmed
temperature limits for each trial or individual.

## Usage

``` r
plot_limits_vs_distance(proj_data, id_col = "fileID", label_points = TRUE)
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
