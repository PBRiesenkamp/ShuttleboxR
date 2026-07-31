# Import a ShuttleSoft data file

Imports a single ShuttleSoft tab-delimited `.txt` file or
comma-separated `.csv` export. A metadata table is optional. For a
single trial, values such as the trial start time can be supplied
directly as arguments. By default, the imported file is prepared for
analysis with
[`file_prepare()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/file_prepare.md).

## Usage

``` r
read_shuttlesoft(
  file,
  metadata = NULL,
  multidat = FALSE,
  trial_start = NULL,
  mass = NULL,
  initial_T = NULL,
  a_value = NULL,
  b_value = NULL,
  prepare = TRUE
)
```

## Arguments

- file:

  Path to one ShuttleSoft `.txt` or `.csv` file. Use
  [`file.choose()`](https://rdrr.io/r/base/file.choose.html) to select a
  file interactively.

- metadata:

  Optional data frame containing one row per file. It must contain
  `file_name` and may contain `trial_start`, `mass`, `initial_T`,
  `a_value`, and `b_value`. The legacy name `initial_temp` is also
  accepted.

- multidat:

  Deprecated compatibility argument. It is no longer needed.

- trial_start:

  Optional clock time at which the experimental trial began, written as
  `"HH:MM:SS"`. When omitted, the first observation is treated as the
  start of the trial, so the whole recording is labelled as trial data.

- mass:

  Optional body mass used only when recalculating `core_T`.

- initial_T:

  Optional initial body temperature used only when recalculating
  `core_T`.

- a_value:

  Optional calibrated coefficient used only when recalculating `core_T`.

- b_value:

  Optional calibrated coefficient used only when recalculating `core_T`.

- prepare:

  Logical. If `TRUE` (the default), run
  [`file_prepare()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/file_prepare.md)
  before returning the data.

## Value

A ShuttleSoft data frame. When `prepare = TRUE`, it is ready for
calculation and plotting functions.

## Details

ShuttleSoft files normally already contain a `core_T` column. The
arguments `mass`, `initial_T`, `a_value`, and `b_value` are therefore
optional and are only needed if body temperature will later be
recalculated with
[`calc_coreT()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT.md).
Direct arguments take priority over values in `metadata`.

## See also

[`read_shuttlesoft_project`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft_project.md),
[`file_prepare`](https://pbriesenkamp.github.io/ShuttleboxR/reference/file_prepare.md),
[`calc_coreT`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT.md)

## Examples

``` r
example_file <- system.file(
  "extdata", "Fish_7_13_2.csv",
  package = "ShuttleboxR"
)
fish <- read_shuttlesoft(example_file)
calc_Tpref(fish, print_results = FALSE)
#> [1] 17.53
```
