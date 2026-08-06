# Changelog

## ShuttleboxR 0.0.0.9016

- Expanded the vignette into a practical function gallery while
  retaining simple, direct one-line examples.
- Added examples of cumulative distance through time with
  [`plot_distance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_distance.md),
  interval summaries with
  [`plot_interval()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_interval.md),
  and movement speed versus core temperature with
  [`plot_speed_coreT()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_speed_coreT.md).
- Added examples of
  [`calc_shuttles()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_shuttles.md),
  [`calc_tot_distance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_tot_distance.md),
  [`calc_occupancy()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_occupancy.md),
  and
  [`calc_track_accuracy()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_track_accuracy.md)
  so users can discover useful single-trial summaries.
- Kept project-level examples prominent, including
  [`plot_distance_vs_shuttles()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_distance_vs_shuttles.md),
  limit-exposure plots, the bivariate matrix, and PCA.
- Added a concise function-finder table and corrected the README example
  for
  [`plot_interval()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_interval.md)
  to include its required `column` argument.

## ShuttleboxR 0.0.0.9015

- Added
  [`calc_Tpercentile_range()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpercentile_range.md)
  to calculate the difference between two user-selected core-temperature
  percentiles. The default 25th and 75th percentiles give the
  interquartile range, describing the width of the central 50% of
  observations.
- Added gravitation-aware, acclimation-aware and custom-window arguments
  consistent with the other thermal metrics.
- Added `Tpercentile_range` to
  [`calc_project_results()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_project_results.md),
  with adjustable `Tpercentile_range_percentiles`.
- Updated the README, vignette, package overview, website index and help
  pages to distinguish `Tpref_range`, `Tpercentile_range`, and the
  distance-based `Tbreadth`.

## ShuttleboxR 0.0.0.9014

- Reworked the README and vignette so use is shown as direct function
  calls rather than assignments, helper objects, manual result printing,
  or code that duplicates work already performed inside package
  functions.
- Made the gravitation workflow consistent throughout the documentation:
  users inspect with
  [`plot_T_segmented()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_T_segmented.md),
  may report the duration with
  [`calc_gravitation()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_gravitation.md),
  and then request settled metrics simply with
  `exclude_gravitation = TRUE`.
- Removed manual reuse of `gravitation_time` from the standard
  fish-review workflow and retained it only as an advanced override in
  the function reference.
- Simplified project-level examples so plots are produced directly.
  Retrieving case tables with `return_cases = TRUE` is now shown once as
  an optional extension rather than as the default workflow.
- Simplified PCA documentation to show only the required explicit
  variable selection and the main returned plots, while retaining
  separate guidance on threshold controls.
- Added concise, direct examples to the help pages for gravitation-aware
  thermal, activity, project-screening, correlation, and PCA functions.

## ShuttleboxR 0.0.0.9013

- Simplified the gravitation workflow throughout the README and
  vignette. Users now only need to set `exclude_gravitation = TRUE`; the
  relevant calculation or plotting function estimates gravitation time
  automatically when no manual value is supplied.
- Removed vignette-only scaffolding such as `use_settled_window`, shared
  argument lists, [`do.call()`](https://rdrr.io/r/base/do.call.html),
  and visible [`tryCatch()`](https://rdrr.io/r/base/conditions.html)
  wrappers that obscured the normal user workflow.
- Clarified that
  [`calc_gravitation()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_gravitation.md)
  is useful for reporting and checking the breakpoint, but it does not
  have to be run before
  [`calc_Tpref()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpref.md),
  [`calc_Tavoid()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tavoid.md),
  [`calc_Tbreadth()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md),
  [`calc_extremes()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_extremes.md),
  [`calc_coreT_variance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT_variance.md),
  or
  [`plot_coreT_histogram()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_coreT_histogram.md).
- Retained `gravitation_time` as an advanced optional override for
  manually checked breakpoints or exact reuse across calculations.
- Clarified that
  [`calc_project_results()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_project_results.md)
  estimates gravitation once per fish under the hood and reuses it for
  all requested metrics.

## ShuttleboxR 0.0.0.9012

- Added gravitation-aware analysis windows to
  [`calc_Tpref()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpref.md),
  [`calc_Tavoid()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tavoid.md),
  [`calc_Tbreadth()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md),
  [`calc_coreT_variance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT_variance.md),
  [`calc_extremes()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_extremes.md),
  [`calc_tot_distance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_tot_distance.md),
  [`calc_shuttles()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_shuttles.md),
  [`calc_occupancy()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_occupancy.md),
  and
  [`plot_coreT_histogram()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_coreT_histogram.md).
- [`calc_gravitation()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_gravitation.md)
  now reports a duration from the selected origin: the dynamic-period
  start when acclimation is excluded, or the recording start otherwise.
- [`plot_T_segmented()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_T_segmented.md)
  now shades the gravitation period, marks its cutoff, and can calculate
  displayed thermal metrics from post-gravitation observations.
- [`calc_project_results()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_project_results.md)
  can exclude gravitation separately for thermal and activity metrics,
  reuses one gravitation estimate per fish, and records analysis-window
  metadata and breakpoint validity.
- When a required project-level gravitation estimate fails, users can
  choose a warning with `NA` gravitation-dependent metrics or a stopping
  error.
- Expanded the README and vignette with a check-then-calculate workflow,
  guidance on interpreting segmented plots, and examples of whole-period
  versus settled-period activity metrics.

## ShuttleboxR 0.0.0.9011

- Replaced the bundled single-trial example with the uploaded
  `Fish_14_13_2.txt` ShuttleSoft recording and updated the README,
  vignette, function examples, and generated help pages accordingly.
- Made the distance-versus-shuttles review plot a more explicit part of
  the project-level vignette workflow.
- Added a rendered bivariate matrix to the vignette using
  [`correlation_matrix()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/correlation_matrix.md),
  together with guidance on reading the scatterplot panels, smooth
  trends, and correlation coefficients and on using the matrix before
  PCA.

## ShuttleboxR 0.0.0.9010

- Replaced the default IQR screen for temperature-limit exposure with a
  direct 10% exposure threshold in
  [`plot_limits_vs_distance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_limits_vs_distance.md),
  [`plot_limits_vs_shuttles()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_limits_vs_shuttles.md),
  and
  [`plot_upper_vs_lower_extremes()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_upper_vs_lower_extremes.md).
- Added `limit_method = "absolute"`, `"quantile"`, or `"iqr"`, together
  with adjustable absolute and quantile thresholds. The absolute method
  is now the default because it remains interpretable in zero-heavy
  project datasets.
- Renamed review categories so they describe threshold exceedance rather
  than implying that any non-zero exposure is biologically high.
- Updated the README and vignette to explain why IQR thresholds can
  collapse to zero and how to set study-appropriate limit-exposure
  thresholds.
- Changed all ggplot-based package figures, including PCA figures
  produced through factoextra, to `theme_classic()` with no gridlines.

## ShuttleboxR 0.0.0.9009

- Added an explicit `variables` argument to
  [`pca()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/pca.md).
  This makes the metrics included in the PCA transparent and prevents
  the PCA from changing silently when new numeric columns are added to a
  project database.
- Added `dbscan_minPts` to control the minimum local neighbourhood size
  used by DBSCAN.
- Added `flag_rule = "both"`, `"either"`, `"mahalanobis"`, or `"dbscan"`
  to control which detections are highlighted as candidates for review.
- Made the default PCA screening more conservative
  (`mahalanobis_th = 0.99`, `dbscan_th = 1.5`, `dbscan_minPts = 4`, and
  `flag_rule = "both"`).
- Added a fish-level `screening` table and `flagged_ids` output while
  retaining the method-level `outliers` table. `outlier_details` now
  indicates whether each fish is selected under the chosen flag rule.
- Revised the README and vignette to explain why PCA results change when
  the selected variables change, how each threshold affects sensitivity,
  and how one-method signals differ from fish identified by both
  methods.

## ShuttleboxR 0.0.0.9008

- Replaced the bundled single-trial example with `Fish_7_13_2`, which
  provides a cleaner and more intuitive illustration of gravitation,
  thermal selection, the core-temperature histogram, and the
  single-trial inspection workflow.
- Updated the README, vignette, function examples, and generated help
  pages to use the new example file.
- Updated the maintenance instructions to render the vignette with
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

## ShuttleboxR 0.0.0.9007

- Fixed vignette-building failures in the project-level screening plots.
  Quantile values are now stored without inherited percentile names, so
  cutoff lookups such as `lower_limit_high`, `distance_low`, and
  `shuttles_high` work correctly.
- Applied the same correction to
  [`plot_upper_vs_lower_extremes()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_upper_vs_lower_extremes.md),
  [`plot_distance_vs_shuttles()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_distance_vs_shuttles.md),
  [`plot_limits_vs_distance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_limits_vs_distance.md),
  and
  [`plot_limits_vs_shuttles()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_limits_vs_shuttles.md).

## ShuttleboxR 0.0.0.9006

- Replaced the original bin-diversity definition of `Tbreadth` with the
  mean absolute difference between pairs of observed core temperatures
  (the Gini mean difference).
- [`calc_Tbreadth()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md)
  now accounts for the actual distance between occupied temperatures,
  does not use histogram bins, and is not centred on `Tpref`.
- Updated
  [`plot_coreT_histogram()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_coreT_histogram.md)
  so its bin width affects only the visual histogram; the displayed
  Tbreadth is calculated from the original observations. The Tpref line
  now shows median core temperature.
- Rewrote the README, vignette, examples and help pages with
  plain-language and worked explanations, including the 50% at 10
  degrees Celsius / 50% at 20 degrees Celsius example.
- Removed the obsolete `Tbreadth_bin_size` argument from
  [`calc_project_results()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_project_results.md).

## ShuttleboxR 0.0.0.9004

- Reframed the README, vignette, package overview and website reference
  index around the package’s three-stage workflow: import and organise,
  calculate metrics, and inspect and troubleshoot.
- Clarified the two connected branches for individual trials and
  complete projects, including the recommended loop from project-level
  screening back to detailed single-trial inspection.
- Expanded the explanation of quality control: project-level outlier
  screens flag candidates for review and do not provide an automatic
  basis for excluding fish.
- Added a metric overview and integrated `Tbreadth` as a complementary
  measure to the percentile-based `Tpref_range`.
- Clarified that ShuttleboxR prepares and explores data before formal
  statistical analysis rather than selecting the inferential analysis
  itself.

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
