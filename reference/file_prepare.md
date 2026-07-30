# Prepare shuttle-box data

Adds elapsed time, date-time, trial phase, dynamic/static state, and
shuttle events to a raw ShuttleSoft data frame. The function can safely
be run on a data frame that has already been prepared.

## Usage

``` r
file_prepare(data)
```

## Arguments

- data:

  A raw shuttle-box data frame, for example as returned by
  [`read_shuttlesoft()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft.md).

## Value

An organised shuttle-box data frame ready for calculation functions.

## Details

When no valid `trial_start` is available, the first observation is used
and the whole recording is labelled as `"trial"`.
