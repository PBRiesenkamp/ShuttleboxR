# Animate the movement of the subject during the trial

This function animates the movement of the subject during the trial

## Usage

``` r
animate_movements(
  data,
  exclude_start_minutes = 0,
  exclude_end_minutes = 0,
  speed = 100
)
```

## Arguments

- data:

  An organised shuttle-box dataframe with corrected core body
  temperature

- exclude_start_minutes:

  Exclusion of time from the start of the trial onwards, in minutes.
  Default is 0

- exclude_end_minutes:

  Exclusion of time from the end of the trial backwards, in minutes.
  Default is 0

- speed:

  Speed setting of animation, default is 100

## Value

Animation of the subject during the trial
