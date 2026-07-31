# ShuttleboxR 0.0.0.9007

- Fixed vignette-building failures in the project-level screening plots. Quantile values are now stored without inherited percentile names, so cutoff lookups such as `lower_limit_high`, `distance_low`, and `shuttles_high` work correctly.
- Applied the same correction to `plot_upper_vs_lower_extremes()`, `plot_distance_vs_shuttles()`, `plot_limits_vs_distance()`, and `plot_limits_vs_shuttles()`.

# ShuttleboxR 0.0.0.9006

- Replaced the original bin-diversity definition of `Tbreadth` with the mean
  absolute difference between pairs of observed core temperatures (the Gini
  mean difference).
- `calc_Tbreadth()` now accounts for the actual distance between occupied
  temperatures, does not use histogram bins, and is not centred on `Tpref`.
- Updated `plot_coreT_histogram()` so its bin width affects only the visual
  histogram; the displayed Tbreadth is calculated from the original
  observations. The Tpref line now shows median core temperature.
- Rewrote the README, vignette, examples and help pages with plain-language and
  worked explanations, including the 50% at 10 degrees Celsius / 50% at 20
  degrees Celsius example.
- Removed the obsolete `Tbreadth_bin_size` argument from
  `calc_project_results()`.

# ShuttleboxR 0.0.0.9004

- Reframed the README, vignette, package overview and website reference index
  around the package's three-stage workflow: import and organise, calculate
  metrics, and inspect and troubleshoot.
- Clarified the two connected branches for individual trials and complete
  projects, including the recommended loop from project-level screening back to
  detailed single-trial inspection.
- Expanded the explanation of quality control: project-level outlier screens
  flag candidates for review and do not provide an automatic basis for
  excluding fish.
- Added a metric overview and integrated `Tbreadth` as a complementary measure
  to the percentile-based `Tpref_range`.
- Clarified that ShuttleboxR prepares and explores data before formal
  statistical analysis rather than selecting the inferential analysis itself.

# ShuttleboxR 0.0.0.9003

- Added a bundled 85-trial project-results database and a complete project-level
  analysis section to the vignette.
- Added `read_project_database()` to import existing summary databases and
  standardise column names used by older ShuttleboxR versions.
- Updated project-level scatter plots and correlation analysis to work directly
  with both current and legacy project-result column names and to return their
  plot or correlation objects.
- Repaired and strengthened `pca()`, including input checks, missing-data
  handling, outlier reporting, and reusable plot outputs.

# ShuttleboxR 0.0.0.9002

- Added a complete getting-started vignette and an included example
  ShuttleSoft file.
- Added a structured `pkgdown` reference index and an automated documentation
  website workflow.
- Expanded the README and added a simple package-maintenance guide.
- `read_shuttlesoft()` and `read_shuttlesoft_project()` now accept both
  tab-delimited `.txt` files and comma-separated `.csv` exports.
- `plot_coreT_histogram()` now reports effective selected thermal breadth,
  uses bin boundaries consistent with `calc_Tbreadth()`, and returns the plot
  object invisibly.

# ShuttleboxR 0.0.0.9001

- Added `calc_Tbreadth()` for effective selected thermal breadth based on the
  frequency distribution of `core_T`.
- Made metadata optional when importing a single ShuttleSoft file.
- Added direct `trial_start`, `mass`, `initial_T`, `a_value`, and `b_value`
  arguments to `read_shuttlesoft()`.
- `read_shuttlesoft()` now prepares imported files by default.
- Made `file_prepare()` robust when no trial start time is supplied.
- Added direct thermal-model arguments to `calc_coreT()` and clarified that
  `a_value` and `b_value` require external calibration.
- Made metadata optional in `read_shuttlesoft_project()`.
- Added `Tbreadth` to `calc_project_results()` and made recalculation of
  `core_T` optional rather than automatic.
