# Plot temperature trajectory and gravitation breakpoint

Shows the temperature trajectory, segmented fit, gravitation period, and
thermal summary metrics.

## Usage

``` r
plot_T_segmented(
  data,
  Tpref_method = "median",
  Tavoid_percentiles = c(0.05, 0.95),
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = TRUE,
  overlay_chamber_temp = TRUE,
  exclude_gravitation = TRUE,
  gravitation_time = NULL
)
```

## Arguments

- data:

  An organised shuttle-box data frame containing time and temperature
  columns.

- Tpref_method:

  Method used to calculate Tpref.

- Tavoid_percentiles:

  Percentiles used to calculate avoidance temperatures.

- exclude_start_minutes:

  Minutes omitted from the start of the selected period.

- exclude_end_minutes:

  Minutes omitted from the end of the recording.

- exclude_acclimation:

  Use the dynamic-period start as the time origin.

- overlay_chamber_temp:

  Overlay warm- and cold-chamber temperatures.

- exclude_gravitation:

  Calculate horizontal thermal metrics after the breakpoint.

- gravitation_time:

  Advanced optional override in hours. Most users can leave this unset
  because gravitation is estimated automatically.

## Value

A `ggplot` object with the gravitation time stored as an attribute.

## Details

The complete selected trajectory remains visible even when the
horizontal metrics are calculated after gravitation.

## Examples

``` r
if (FALSE) { # \dontrun{
fish <- read_shuttlesoft(file.choose())
plot_T_segmented(fish)
} # }
```
