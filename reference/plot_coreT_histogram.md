# Plot the distribution of core body temperatures

Plots the frequency distribution of selected core temperatures and can
summarise Tpref and Tbreadth.

## Usage

``` r
plot_coreT_histogram(
  data,
  bin_size = 0.1,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  exclude_acclimation = FALSE,
  show_Tpref = TRUE,
  show_Tbreadth = TRUE,
  exclude_gravitation = FALSE,
  gravitation_time = NULL
)
```

## Arguments

- data:

  An organised shuttle-box data frame containing `core_T`.

- bin_size:

  Displayed histogram-bin width in degrees Celsius.

- exclude_start_minutes:

  Minutes omitted from the start of the selected period.

- exclude_end_minutes:

  Minutes omitted from the end of the recording.

- exclude_acclimation:

  Use only the dynamic period.

- show_Tpref:

  Show median Tpref.

- show_Tbreadth:

  Show Tbreadth in the subtitle.

- exclude_gravitation:

  Plot only post-gravitation observations.

- gravitation_time:

  Advanced optional override in hours. Most users can leave this unset;
  when `exclude_gravitation = TRUE`, gravitation is estimated
  automatically.

## Value

Invisibly returns a `ggplot` object.

## See also

[`calc_Tbreadth`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md),
[`calc_Tpercentile_range`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpercentile_range.md),
[`calc_gravitation`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_gravitation.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fish <- read_shuttlesoft(file.choose())
plot_coreT_histogram(fish, exclude_gravitation = TRUE)
} # }
```
