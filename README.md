# ShuttleboxR

ShuttleboxR provides a structured workflow for moving from raw shuttle-box
recordings to a checked, interpretable dataset that is ready for statistical
analysis.

The package is organised around **three consecutive tasks**:

1. **Import and organise** the raw data.
2. **Calculate shuttle-box metrics** using consistent definitions.
3. **Inspect and troubleshoot** the data before formal analysis.

The same logic is applied at two complementary levels:

- the **single-trial branch**, used to calculate and inspect the results for one
  fish; and
- the **project branch**, used after all fish have been processed to compare
  metrics across the study and flag unusual trials for closer inspection.

```text
Raw shuttle-box recordings
        |
        +-- Single-trial branch
        |      1. Import and organise one trial
        |      2. Calculate thermal and behavioural metrics
        |      3. Inspect system performance and fish behaviour
        |
        +-- Project branch
               1. Import all trials or an existing results database
               2. Compile one row of metrics per fish
               3. Inspect distributions, relationships and multivariate patterns
                              |
                              v
                    Flag trials for review
                              |
                              v
                    Return to the single-trial plots
```

ShuttleboxR supports quality control and data exploration. It does **not**
automatically decide that a fish should be excluded, and it does not replace the
statistical analysis appropriate to a study. A trial that appears unusual at the
project level should be checked against its raw temperature, tracking and
behavioural records, and interpreted in the context of the species and
experimental design.

## Installation

Install the current GitHub version with:

```r
install.packages("remotes")  # only needed once

remotes::install_github(
  "PBRiesenkamp/ShuttleboxR",
  build_vignettes = TRUE
)
```

Then load the package:

```r
library(ShuttleboxR)
```

## Choose your starting point

The correct import function depends on what kind of file or files you already
have. ShuttleboxR supports three starting points:

| Starting data | Import function | Resulting object | Next step |
|---|---|---|---|
| One raw ShuttleSoft trial for one fish | `read_shuttlesoft()` | `fish`: time-by-time observations for one fish | Calculate and inspect single-trial metrics |
| A folder of raw ShuttleSoft trials, one file per fish | `read_shuttlesoft_project()` | `all_fish`: a named list of raw fish trials | Use `calc_project_results()` to create `project_data` |
| One existing project-summary CSV, with one row per fish | `read_project_database()` | `project_data`: the project-level summary table | Go directly to project-level inspection |

### One raw trial file

```r
fish <- read_shuttlesoft(file.choose())
```

Use this route when the selected file contains the time-by-time ShuttleSoft
recording for one fish.

### A folder of raw trial files

```r
all_fish <- read_shuttlesoft_project(
  directory = choose.dir()
)

project_data <- calc_project_results(all_fish)
```

The Windows dialogue created by `choose.dir()` displays folders rather than the
individual `.txt` or `.csv` files inside them. Navigate to the folder containing
all raw trial files and click **Select Folder**. `all_fish` is a list of raw
trials; `project_data` is the resulting one-row-per-fish summary table.

### An existing project-summary file

```r
project_data <- read_project_database(file.choose())
```

Use this route when the selected CSV already contains one row per fish and
summary columns such as `Tpref`, `nr_shuttles`, `tot_distance` and
`t_near_limits`. Do not pass an existing project-summary table through
`read_shuttlesoft()` or `read_shuttlesoft_project()`, because those functions
expect raw time-series recordings.

In brief:

```text
One raw fish file     -> read_shuttlesoft()         -> fish
Folder of raw files   -> read_shuttlesoft_project() -> all_fish
                                                |
                                                v
                                     calc_project_results()
                                                |
                                                v
                                           project_data

Existing summary CSV -> read_project_database()    -> project_data
```

## Branch 1: analyse and inspect one fish

### 1. Import and organise

Select a ShuttleSoft `.txt` file or comma-separated `.csv` export:

```r
fish <- read_shuttlesoft(file.choose())
```

`read_shuttlesoft()` imports and prepares the recording automatically. It adds
elapsed time, labels trial phases and identifies shuttles between chambers. The
existing `core_T` values produced by ShuttleSoft are retained by default.

A complete ShuttleSoft text recording is included with the package and is used
throughout the vignette:

```r
example_file <- system.file(
  "extdata",
  "Fish_14_13_2.txt",
  package = "ShuttleboxR"
)

fish <- read_shuttlesoft(example_file)
```

When the recording includes a separate acclimation period, provide the clock
time at which the dynamic trial began:

```r
fish <- read_shuttlesoft(
  file.choose(),
  trial_start = "13:30:00"
)
```

The structure can be checked with:

```r
inspect(fish)
```

### 2. Inspect gravitation before calculating thermal metrics

The early dynamic phase often represents a transition from the starting
conditions towards the fish's settled thermal region. Including this
**gravitation period** can pull Tpref towards the starting temperature and
inflate Tbreadth, avoidance percentiles, core-temperature variation, and time
near limits.

First inspect the trial and calculate gravitation time:

```r
grav_time <- calc_gravitation(
  fish,
  exclude_acclimation = TRUE,
  print_results = FALSE
)

plot_T_segmented(
  fish,
  exclude_acclimation = TRUE,
  gravitation_time = grav_time,
  exclude_gravitation = TRUE
)
```

`grav_time` is a duration in hours. With `exclude_acclimation = TRUE`, it is
added to the start of the dynamic period; with `FALSE`, it is added to the start
of the complete recording. When `exclude_start_minutes` is also supplied, the
later of the custom start and the gravitation cutoff is used.

After checking that the breakpoint is plausible, reuse the same value:

```r
calc_Tpref(
  fish,
  exclude_acclimation = TRUE,
  exclude_gravitation = TRUE,
  gravitation_time = grav_time
)
calc_Tavoid(
  fish,
  exclude_acclimation = TRUE,
  exclude_gravitation = TRUE,
  gravitation_time = grav_time
)
calc_Tbreadth(
  fish,
  exclude_acclimation = TRUE,
  exclude_gravitation = TRUE,
  gravitation_time = grav_time
)
calc_extremes(
  fish,
  exclude_acclimation = TRUE,
  exclude_gravitation = TRUE,
  gravitation_time = grav_time
)
calc_coreT_variance(
  fish,
  variance_type = "std_deviation",
  exclude_acclimation = TRUE,
  exclude_gravitation = TRUE,
  gravitation_time = grav_time
)
```

These values describe **settled thermal behaviour**. Distance, shuttling, and
occupancy can either cover the complete selected period or only the settled
period, depending on the question:

```r
# Whole dynamic-period activity
calc_shuttles(fish, exclude_acclimation = TRUE)

# Activity during settled thermoregulation
calc_shuttles(
  fish,
  exclude_acclimation = TRUE,
  exclude_gravitation = TRUE,
  gravitation_time = grav_time
)
```

The thermal metrics answer related but distinct questions:

- `Tpref` summarises the central selected temperature; the default is median
  `core_T`.
- `Tavoid_lower` and `Tavoid_upper` are lower and upper percentiles of the
  selected `core_T` distribution.
- `Tpref_range` is the difference between those avoidance temperatures.
- `Tbreadth` describes how far apart the temperatures experienced by the fish
  were. It is the average absolute difference between temperatures at two
  randomly selected observations from the analysed period.

### What does Tbreadth mean?

Imagine choosing two moments from the analysed part of the trial and asking how
different the fish's core temperature was. Repeat that for every possible pair
and take the average. That average is `Tbreadth`.

- A fish that remains at nearly one temperature has a Tbreadth close to 0 °C.
- Equal time at 20 °C and 21 °C gives a Tbreadth of 0.5 °C.
- Equal time at 10 °C and 20 °C gives a Tbreadth of 5 °C.

`Tbreadth` is **not centred on Tpref**, does not define lower and upper
boundaries, and does not depend on histogram bin size. It summarises overall
spread, while `plot_coreT_histogram()` shows whether that spread is symmetrical,
skewed, or split into multiple peaks.

```r
plot_coreT_histogram(
  fish,
  exclude_acclimation = TRUE,
  exclude_gravitation = TRUE,
  gravitation_time = grav_time
)
```

### 3. Inspect the trial

Single-trial plots help answer two different quality-control questions.

**Did the shuttle-box and tracking system work as intended?**

```r
plot_T_gradient(fish)
plot_tracking(fish)
plot_T_segmented(fish)
```

**Did the fish display interpretable behaviour?**

```r
plot_coreT_histogram(fish)
plot_heatmap(fish)
plot_distance(fish)
plot_interval(fish)
plot_speed_coreT(fish)
animate_movements(fish)
```

These plots can reveal interruptions in temperature control, poor tracking,
prolonged inactivity, extensive time near system limits, doorway use or other
patterns that deserve closer examination.

## Branch 2: inspect a complete project

### 1. Create or import `project_data`

There are two routes into the project-level workflow.

**Route A: start from a folder of raw trial files**

Place the raw `.txt` or `.csv` files for all fish in one folder and import them
as a named list:

```r
all_fish <- read_shuttlesoft_project(
  directory = choose.dir()
)
```

The folder-selection window shows folders, not the individual trial files.
Select the folder containing all raw recordings. A metadata table is optional
and is useful when files have different trial start times:

```r
metadata <- data.frame(
  file_name = c("Fish_1.txt", "Fish_2.txt"),
  trial_start = c("10:30:00", "14:15:00")
)

all_fish <- read_shuttlesoft_project(
  metadata = metadata,
  directory = choose.dir()
)
```

Calculate one row of summary metrics per fish:

```r
project_data <- calc_project_results(
  all_fish,
  exclude_acclimation = TRUE,
  Tpref_method = "median",
  Tavoid_percentiles = c(0.05, 0.95),
  exclude_gravitation_thermal = TRUE,
  exclude_gravitation_activity = FALSE,
  gravitation_failure = "warn"
)
```


Here the thermal distribution metrics are calculated after gravitation, while
distance, shuttling, and occupancy retain the complete dynamic period. The
output records whether gravitation was valid and the start time used for each
metric group. When a breakpoint cannot be estimated, `gravitation_failure =
"warn"` records `NA` for metrics that require a post-gravitation window rather
than silently using the full trial.

**Route B: start from an existing project-summary CSV**

When the CSV already contains one row per fish and the required summary
metrics, import it directly:

```r
project_data <- read_project_database(file.choose())
```

This route skips `read_shuttlesoft_project()` and `calc_project_results()`. It
does not calculate metrics that are absent from the summary file; for example,
`Tbreadth` must be recalculated from the original raw trials if it is not
already present.
It also cannot determine retrospectively whether gravitation was excluded. The
summary file should therefore retain columns or accompanying metadata that
record the calculation window used.

### 2. Flag unusual trials for individual review

Start with the distributions of key metrics:

```r
possible_outliers <- plot_histograms(project_data)
```

Then inspect relationships that can distinguish different kinds of unusual
behaviour. Distance versus shuttles is a useful first comparison because it
separates overall movement from movement between chambers:

```r
distance_shuttle_review <- plot_distance_vs_shuttles(
  project_data,
  highlight_cases = TRUE,
  return_cases = TRUE
)
distance_shuttle_review$plot

plot_limits_vs_distance(
  project_data,
  highlight_cases = TRUE,
  limit_threshold = 10,
  label_points = FALSE
)
plot_limits_vs_shuttles(
  project_data,
  highlight_cases = TRUE,
  limit_threshold = 10,
  label_points = FALSE
)
plot_upper_vs_lower_extremes(
  project_data,
  lower_limit_threshold = 10,
  upper_limit_threshold = 10
)
```

The default limit-exposure rule is direct and interpretable: a fish is flagged
when more than 10% of analysed observations occurred near the programmed
limits. This avoids IQR thresholds collapsing to zero in projects where most
fish never approach either limit. Adjust `limit_threshold`,
`lower_limit_threshold`, and `upper_limit_threshold` to match the study design.
Project-relative alternatives remain available through
`limit_method = "quantile"` or `limit_method = "iqr"`.

A bivariate matrix shows all selected pairwise relationships together. The
lower panels contain scatterplots and smooth trends, while the upper panels show
Pearson correlations. It is useful for seeing redundant metrics and unexpected
combinations before running PCA:

```r
correlation_matrix(
  project_data,
  columns = c(
    "Tpref", "Tpref_range", "grav_time",
    "tot_distance", "nr_shuttles", "t_near_limits"
  )
)
```

PCA then provides a complementary multivariate overview:

```r
project_pca <- pca(
  project_data,
  variables = c(
    "Tpref", "Tavoid_lower", "Tavoid_upper",
    "tot_distance", "nr_shuttles", "t_near_limits"
  ),
  mahalanobis_th = 0.99,
  dbscan_th = 1.5,
  dbscan_minPts = 4,
  flag_rule = "both",
  print_labels = FALSE
)

project_pca$plots$biplot
project_pca$screening
project_pca$outlier_details
```


The PCA variables are supplied explicitly because the PCA can change greatly if
variables are added or removed, even when the project dataset contains the same
fish. The default `flag_rule = "both"` highlights only fish identified by both
Mahalanobis distance and DBSCAN. Use `flag_rule = "either"` for a more sensitive
screen. Increasing `mahalanobis_th` or `dbscan_th` will generally reduce the
number of fish flagged; `dbscan_minPts` controls the minimum local neighbourhood
size. These settings identify fish for closer inspection, not automatic
exclusion.

When the project database was generated with the current version of
`calc_project_results()`, `Tbreadth` can also be included in these project-level
comparisons. Older summary databases must be regenerated from the raw trial
files before `Tbreadth` is available.

The output identifies **candidates for review**, not automatic exclusions. The
recommended next step is to locate each flagged fish in `all_fish` and inspect
its raw trial:

```r
fish_to_check <- all_fish[["Fish_17.txt"]]

plot_T_gradient(fish_to_check)
plot_tracking(fish_to_check)
plot_coreT_histogram(fish_to_check)
plot_heatmap(fish_to_check)
```

A fish should only be excluded when there is a clear, documented reason that the
trial is technically flawed or does not provide a valid measure of the intended
behaviour. Unusual behaviour can also be genuine biological variation.

## Recalculating core temperature is optional

ShuttleSoft files normally already contain `core_T`. Only use `calc_coreT()`
when those values need to be replaced and appropriate thermal-lag coefficients
are available:

```r
fish <- calc_coreT(
  fish,
  mass = 12.4,
  a_value = 0.05,
  b_value = -0.25
)
```

The `a_value` and `b_value` coefficients cannot be inferred from the recording.
They must come from an appropriate calibration or published source.

## Full guide

After installation, open the worked vignette with:

```r
vignette("ShuttleboxR", package = "ShuttleboxR")
```

Individual function documentation is available in R, for example:

```r
?calc_Tbreadth
help(package = "ShuttleboxR")
```
