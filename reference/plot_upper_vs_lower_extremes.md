# Plot upper versus lower temperature-limit exposure

Compares the percentage of observations spent near the lower and upper
programmed temperature limits. The plot can highlight fish exceeding a
transparent threshold at the upper limit, lower limit, or both. This is
a screening step that should be followed by inspection of the original
trial.

## Usage

``` r
plot_upper_vs_lower_extremes(
  proj_data,
  id_col = "fileID",
  label_points = TRUE,
  highlight_cases = TRUE,
  iqr_multiplier = 1.5,
  return_cases = FALSE,
  limit_method = c("absolute", "quantile", "iqr"),
  lower_limit_threshold = 10,
  upper_limit_threshold = 10,
  limit_quantile = 0.95
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

  Logical. Highlight potential review cases. Default is `TRUE`.

- iqr_multiplier:

  Multiplier used when `limit_method = "iqr"` to define each cutoff as
  `Q3 + multiplier * IQR`. Default is 1.5.

- limit_method:

  Method used to define elevated exposure at each limit. One of
  `"absolute"` (default), `"quantile"`, or `"iqr"`.

- lower_limit_threshold:

  Percentage threshold for lower-limit exposure when
  `limit_method = "absolute"`. Default is 10.

- upper_limit_threshold:

  Percentage threshold for upper-limit exposure when
  `limit_method = "absolute"`. Default is 10.

- limit_quantile:

  Project quantile used separately for lower- and upper-limit exposure
  when `limit_method = "quantile"`. Default is 0.95.

- return_cases:

  Logical. Return a list containing the plot, highlighted cases and
  cutoffs. Default is `FALSE`.

## Value

A `ggplot2` plot invisibly, or a list with `plot`, `cases`, `cutoffs`,
and `limit_method` when `return_cases = TRUE`.
