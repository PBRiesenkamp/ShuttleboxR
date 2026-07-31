# Calculate temperature preference

Calculates the centre of the selected core-temperature distribution.

## Usage

``` r
calc_Tpref(
  data,
  method = c("median", "mean", "mode"),
  exclude_acclimation = FALSE,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  print_results = TRUE,
  exclude_gravitation = FALSE,
  gravitation_time = NULL
)
```

## Arguments

- data:

  An organised shuttle-box data frame containing `core_T`.

- method:

  Calculation method: `"median"`, `"mean"`, or `"mode"`.

- exclude_acclimation:

  Use only the dynamic period.

- exclude_start_minutes:

  Minutes omitted from the start of the selected period.

- exclude_end_minutes:

  Minutes omitted from the end of the recording.

- print_results:

  Print the result.

- exclude_gravitation:

  Exclude the transitional gravitation period.

- gravitation_time:

  Optional gravitation duration in hours, usually from
  [`calc_gravitation()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_gravitation.md).

## Value

A single temperature preference in degrees Celsius.

## Details

When gravitation is excluded, its duration is added to the
dynamic-period start if acclimation is excluded, or to the recording
start otherwise.
