# Calculate exposure near programmed temperature limits

Calculates limit exposure within the selected analysis window.

## Usage

``` r
calc_extremes(
  data,
  threshold = 0.2 * (max(data$max_T, na.rm = TRUE) - max(data$min_T, na.rm = TRUE)),
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = FALSE,
  print_results = TRUE,
  exclude_gravitation = FALSE,
  gravitation_time = NULL
)
```

## Arguments

- data:

  An organised shuttle-box data frame containing temperature and limit
  columns.

- threshold:

  Width of each extreme-temperature zone in degrees Celsius.

- exclude_start_minutes:

  Minutes omitted from the start of the selected period.

- exclude_end_minutes:

  Minutes omitted from the end of the recording.

- exclude_acclimation:

  Use only the dynamic period.

- print_results:

  Print the results.

- exclude_gravitation:

  Exclude the transitional gravitation period.

- gravitation_time:

  Advanced optional override in hours. Most users can leave this unset;
  when `exclude_gravitation = TRUE`, gravitation is estimated
  automatically.

## Value

Percentages near the lower and upper programmed limits.

## Examples

``` r
if (FALSE) { # \dontrun{
fish <- read_shuttlesoft(file.choose())
calc_extremes(fish, exclude_gravitation = TRUE)
} # }
```
