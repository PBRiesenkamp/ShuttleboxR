# Changelog

## ShuttleboxR 0.0.0.9003

- Added a bundled 85-trial project-results database and a complete
  project-level analysis section to the vignette.
- Added
  [`read_project_database()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_project_database.md)
  to import existing summary databases and standardise column names used
  by older ShuttleboxR versions.
- Updated project-level scatter plots and correlation analysis to work
  directly with both current and legacy project-result column names and
  to return their plot or correlation objects.
- Repaired and strengthened
  [`pca()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/pca.md),
  including input checks, missing-data handling, outlier reporting, and
  reusable plot outputs.

## ShuttleboxR 0.0.0.9002

- Added a complete getting-started vignette and an included example
  ShuttleSoft file.
- Added a structured `pkgdown` reference index and an automated
  documentation website workflow.
- Expanded the README and added a simple package-maintenance guide.
- [`read_shuttlesoft()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft.md)
  and
  [`read_shuttlesoft_project()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft_project.md)
  now accept both tab-delimited `.txt` files and comma-separated `.csv`
  exports.
- [`plot_coreT_histogram()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_coreT_histogram.md)
  now reports effective selected thermal breadth, uses bin boundaries
  consistent with
  [`calc_Tbreadth()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md),
  and returns the plot object invisibly.

## ShuttleboxR 0.0.0.9001

- Added
  [`calc_Tbreadth()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md)
  for effective selected thermal breadth based on the frequency
  distribution of `core_T`.
- Made metadata optional when importing a single ShuttleSoft file.
- Added direct `trial_start`, `mass`, `initial_T`, `a_value`, and
  `b_value` arguments to
  [`read_shuttlesoft()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft.md).
- [`read_shuttlesoft()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft.md)
  now prepares imported files by default.
- Made
  [`file_prepare()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/file_prepare.md)
  robust when no trial start time is supplied.
- Added direct thermal-model arguments to
  [`calc_coreT()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT.md)
  and clarified that `a_value` and `b_value` require external
  calibration.
- Made metadata optional in
  [`read_shuttlesoft_project()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft_project.md).
- Added `Tbreadth` to
  [`calc_project_results()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_project_results.md)
  and made recalculation of `core_T` optional rather than automatic.
