# Plot upper versus lower temperature-limit exposure

Compares the percentage of observations spent near the lower and upper
programmed temperature limits. The plot can highlight fish with
unusually high exposure to the upper limit, lower limit, or both. This
is a screening step that should be followed by inspection of the
original trial.

## Usage

``` r
plot_upper_vs_lower_extremes(
  proj_data,
  id_col = "fileID",
  label_points = TRUE,
  highlight_cases = TRUE,
  iqr_multiplier = 1.5,
  return_cases = FALSE
)
```

## Arguments

- proj_data:

  Project-results data frame containing `t_near_min` and `t_near_max`.

- id_col:

  Identifier column used for labels. Default is `"fileID"`.

- label_points:

  Logical. Label highlighted points. Default is `TRUE`.

- highlight_cases:

  Logical. Highlight potential review cases using an IQR rule separately
  for the upper and lower limits. Default is `TRUE`.

- iqr_multiplier:

  Multiplier in `Q3 + multiplier * IQR`. Default is 1.5.

- return_cases:

  Logical. Return a list containing the plot, highlighted cases and
  cutoffs. Default is `FALSE`.

## Value

A `ggplot2` plot invisibly, or a list with `plot`, `cases` and `cutoffs`
when `return_cases = TRUE`.
