# Calculate the number of shuttles

Counts shuttles within the selected analysis window.

## Usage

``` r
calc_shuttles(
  data,
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

  An organised shuttle-box data frame containing `shuttle`.

- exclude_start_minutes:

  Minutes omitted from the start of the selected period.

- exclude_end_minutes:

  Minutes omitted from the end of the recording.

- exclude_acclimation:

  Use only the dynamic period.

- print_results:

  Print the result.

- exclude_gravitation:

  Exclude the transitional gravitation period.

- gravitation_time:

  Advanced optional override in hours. Most users can leave this unset;
  when `exclude_gravitation = TRUE`, gravitation is estimated
  automatically.

## Value

The number of chamber transitions.

## Examples

``` r
if (FALSE) { # \dontrun{
fish <- read_shuttlesoft(file.choose())
calc_shuttles(fish)
calc_shuttles(fish, exclude_gravitation = TRUE)
} # }
```
