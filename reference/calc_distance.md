# Calculate the cumulative distance

This function calculates the distance covered for each observation
during the trial.

## Usage

``` r
calc_distance(data, pixel_to_cm = TRUE)
```

## Arguments

- data:

  An organised shuttle-box dataframe with corrected core body
  temperature

- pixel_to_cm:

  if TRUE, converts coordinates with pixels as unit to coordinates with
  cm as unit. Default is TRUE

## Value

A shuttle-box dataframe with distance calculated for each observation
