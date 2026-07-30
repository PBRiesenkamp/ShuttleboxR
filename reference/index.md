# Package index

## Import, prepare, and inspect data

- [`read_shuttlesoft()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft.md)
  : Import a ShuttleSoft data file
- [`read_shuttlesoft_project()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft_project.md)
  : Import all ShuttleSoft files in a directory
- [`read_project_database()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_project_database.md)
  : Read a ShuttleboxR project-results database
- [`file_prepare()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/file_prepare.md)
  : Prepare shuttle-box data
- [`compile_project_data()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/compile_project_data.md)
  : Compile all trials into a single datafile
- [`inspect()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/inspect.md)
  : Inspect imported shuttle-box data

## Thermal metrics

- [`calc_Tpref()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpref.md)
  : Calculate the temperature preference
- [`calc_Tavoid()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tavoid.md)
  : Calculate the avoidance temperatures
- [`calc_Tbreadth()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md)
  : Calculate effective selected thermal breadth
- [`calc_coreT()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT.md)
  : Recalculate core body temperature
- [`calc_coreT_variance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT_variance.md)
  : Calculate the temperature preference
- [`calc_extremes()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_extremes.md)
  : Calculate the time spent near the extremes
- [`calc_gravitation()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_gravitation.md)
  : Calculate the gravitation period

## Movement, occupancy, and tracking

- [`calc_distance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_distance.md)
  : Calculate the cumulative distance
- [`calc_tot_distance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_tot_distance.md)
  : Calculate the total distance
- [`calc_shuttles()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_shuttles.md)
  : Calculate the number of shuttles
- [`calc_occupancy()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_occupancy.md)
  : Calculate the occupancy times
- [`calc_track_accuracy()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_track_accuracy.md)
  : Calculate the tracking accuracy

## Single-trial plots

- [`plot_T_gradient()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_T_gradient.md)
  : Plot the temperatures in each side of the shuttlebox over time
- [`plot_T_segmented()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_T_segmented.md)
  : Plot the core body temperature during the trial
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

## Project-level summaries and plots

- [`calc_project_results()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_project_results.md)
  : Calculate shuttle-box metrics for all trials
- [`correlation_matrix()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/correlation_matrix.md)
  : Plot a correlation matrix of selected shuttle-box metrics
- [`plot_histograms()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_histograms.md)
  : Plot frequency distributions of key shuttle-box metrics
- [`plot_distance_vs_shuttles()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_distance_vs_shuttles.md)
  : Plot distance versus shuttles across project data
- [`plot_limits_vs_distance()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_limits_vs_distance.md)
  : Plot distance versus time spent near limits across project data
- [`plot_limits_vs_shuttles()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_limits_vs_shuttles.md)
  : Plot time spent near limits versus shuttles across project data
- [`pca()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/pca.md)
  : Perform principal component analysis on project data
