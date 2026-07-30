# Import all ShuttleSoft files in a directory

Imports every ShuttleSoft `.txt` and `.csv` file in a directory and
returns them as a named list. A metadata table is optional. When
supplied, it may provide different trial start times or thermal-model
values for each file.

## Usage

``` r
read_shuttlesoft_project(metadata = NULL, directory = getwd(), prepare = TRUE)
```

## Arguments

- metadata:

  Optional data frame containing `file_name` and any of `trial_start`,
  `mass`, `initial_T`, `a_value`, or `b_value`.

- directory:

  Directory containing the ShuttleSoft files. The default is the current
  working directory.

- prepare:

  Logical. If `TRUE` (the default), prepare each file with
  [`file_prepare()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/file_prepare.md).

## Value

A named list of imported ShuttleSoft data frames.

## Details

Files are imported in alphabetical order. If `metadata` is supplied,
values are matched to each file using the `file_name` column.

## See also

[`read_shuttlesoft`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft.md),
[`calc_project_results`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_project_results.md)
