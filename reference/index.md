# Package index

## Single trials: import and organise

Read, prepare and validate one shuttle-box recording.

- [`read_shuttlesoft()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft.md)
  : Import a ShuttleSoft data file
- [`file_prepare()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/file_prepare.md)
  : Prepare shuttle-box data
- [`inspect()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/inspect.md)
  : Inspect imported shuttle-box data

## Single trials: calculate metrics

Calculate thermal, behavioural, movement and tracking metrics for one
fish.

- [`calc_coreT()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT.md)
  : Recalculate core body temperature
- [`calc_distance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_distance.md)
  : Calculate the cumulative distance
- [`calc_Tpref()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpref.md)
  : Calculate temperature preference
- [`calc_Tavoid()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tavoid.md)
  : Calculate avoidance temperatures
- [`calc_Tbreadth()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md)
  : Calculate selected thermal breadth
- [`calc_extremes()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_extremes.md)
  : Calculate exposure near programmed temperature limits
- [`calc_tot_distance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_tot_distance.md)
  : Calculate total distance moved
- [`calc_gravitation()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_gravitation.md)
  : Calculate gravitation time
- [`calc_shuttles()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_shuttles.md)
  : Calculate the number of shuttles
- [`calc_occupancy()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_occupancy.md)
  : Calculate chamber occupancy
- [`calc_coreT_variance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT_variance.md)
  : Calculate variation in core body temperature
- [`calc_track_accuracy()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_track_accuracy.md)
  : Calculate tracking accuracy

## Single trials: inspect and troubleshoot

Check temperature control, tracking and fish behaviour within a trial.

- [`plot_T_gradient()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_T_gradient.md)
  : Plot the temperatures in each side of the shuttlebox over time
- [`plot_T_segmented()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_T_segmented.md)
  : Plot temperature trajectory and gravitation breakpoint
- [`plot_coreT_histogram()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_coreT_histogram.md)
  : Plot the distribution of core body temperatures
- [`plot_distance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_distance.md)
  : Plot the cumulative distance during the trial
- [`plot_heatmap()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_heatmap.md)
  : Plot number of lost tracks per interval
- [`plot_tracking()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_tracking.md)
  : Plot number of lost tracks per interval
- [`plot_speed_coreT()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_speed_coreT.md)
  : Plot relation between velocity and core body temperature
- [`animate_movements()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/animate_movements.md)
  : Animate the movement of the subject during the trial
- [`plot_histogram()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_histogram.md)
  : Plot a histogram of a chosen shuttle-box metric
- [`plot_interval()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_interval.md)
  : Plot the interval means for a selected column over time

## Projects: import and summarise

Combine trials and create or read a one-row-per-fish results database.

- [`read_shuttlesoft_project()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft_project.md)
  : Import all ShuttleSoft files in a directory
- [`compile_project_data()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/compile_project_data.md)
  : Compile all trials into a single datafile
- [`calc_project_results()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_project_results.md)
  : Calculate shuttle-box metrics for all trials
- [`read_project_database()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_project_database.md)
  : Read a ShuttleboxR project-results database

## Projects: inspect and flag trials

Explore variation across fish and identify candidates for single-trial
review.

- [`plot_histograms()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_histograms.md)
  : Plot frequency distributions of key shuttle-box metrics
- [`plot_distance_vs_shuttles()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_distance_vs_shuttles.md)
  : Plot distance versus shuttles across project data
- [`plot_limits_vs_distance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_limits_vs_distance.md)
  : Plot distance versus time spent near limits across project data
- [`plot_limits_vs_shuttles()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_limits_vs_shuttles.md)
  : Plot time spent near limits versus shuttles across project data
- [`plot_upper_vs_lower_extremes()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_upper_vs_lower_extremes.md)
  : Plot upper versus lower temperature-limit exposure
- [`correlation_matrix()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/correlation_matrix.md)
  : Plot a correlation matrix of selected shuttle-box metrics
- [`pca()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/pca.md)
  : Perform principal component analysis on project data
