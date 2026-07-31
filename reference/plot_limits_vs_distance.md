# Plot distance versus time spent near limits across project data

Plots total movement distance against the percentage of observations
spent close to the programmed temperature limits. Optional review guides
highlight trials exceeding a transparent limit-exposure threshold and
trials with unusually low or high movement within the project.
Highlighted cases require inspection of the original trial and are not
automatic exclusions.

## Usage

``` r
plot_limits_vs_distance(
  proj_data,
  id_col = "fileID",
  label_points = TRUE,
  highlight_cases = FALSE,
  lower_quantile = 0.05,
  upper_quantile = 0.95,
  limits_iqr_multiplier = 1.5,
  return_cases = FALSE,
  limit_method = c("absolute", "quantile", "iqr"),
  limit_threshold = 10,
  limit_quantile = 0.95
)
```

## Arguments

- proj_data:

  Project-results data frame.

- id_col:

  Identifier column used for labels. Default is `"fileID"`.

- label_points:

  Logical. Label points. When `highlight_cases = TRUE`, only highlighted
  cases are labelled. Default is `TRUE`.

- highlight_cases:

  Logical. Highlight potential review cases. Default is `FALSE`.

- lower_quantile:

  Lower movement quantile used for screening. Default is 0.05.

- upper_quantile:

  Upper movement quantile used for screening. Default is 0.95.

- limits_iqr_multiplier:

  Multiplier used when `limit_method = "iqr"` to define the cutoff as
  `Q3 + multiplier * IQR`. Default is 1.5.

- limit_method:

  Method used to define elevated limit exposure. One of `"absolute"`
  (default), `"quantile"`, or `"iqr"`. The absolute method is
  recommended for zero-heavy datasets because it retains a direct
  biological interpretation.

- limit_threshold:

  Percentage threshold used when `limit_method = "absolute"`. Default is
  10, meaning more than 10 percent of analysed observations near either
  programmed limit.

- limit_quantile:

  Project quantile used when `limit_method = "quantile"`. Default is
  0.95.

- return_cases:

  Logical. Return a list containing the plot, highlighted cases and
  cutoffs. Default is `FALSE`.

## Value

A `ggplot2` plot invisibly, or a list with `plot`, `cases`, `cutoffs`,
and `limit_method` when `return_cases = TRUE`.
