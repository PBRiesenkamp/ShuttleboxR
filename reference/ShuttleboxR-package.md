# ShuttleboxR: Data Exploration and Analysis for Shuttle-Box Temperature Experiments

ShuttleboxR imports, prepares, checks, analyses, and visualises data
produced by ShuttleSoft shuttle-box experiments. It supports both
single-trial and multi-file workflows.

## Typical workflow

1.  Import one file with
    [`read_shuttlesoft()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft.md)
    or a folder of files with
    [`read_shuttlesoft_project()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/read_shuttlesoft_project.md).

2.  Inspect the imported data with
    [`inspect()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/inspect.md).

3.  Calculate thermal and behavioural metrics such as
    [`calc_Tpref()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tpref.md),
    [`calc_Tavoid()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tavoid.md),
    [`calc_Tbreadth()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_Tbreadth.md),
    [`calc_shuttles()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_shuttles.md),
    and
    [`calc_occupancy()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_occupancy.md).

4.  Visualise individual trials with functions such as
    [`plot_T_segmented()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_T_segmented.md),
    [`plot_coreT_histogram()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_coreT_histogram.md),
    [`plot_tracking()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_tracking.md),
    and
    [`plot_heatmap()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_heatmap.md).

5.  Summarise a complete project with
    [`calc_project_results()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_project_results.md).

## Core temperature

ShuttleSoft files normally already include `core_T`. Recalculation with
[`calc_coreT()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT.md)
is optional and requires calibrated thermal-lag parameters.

## Getting started

Run
[`vignette("ShuttleboxR", package = "ShuttleboxR")`](https://pbriesenkamp.github.io/ShuttleboxR/articles/ShuttleboxR.md)
for a complete worked example using the file included with the package.

## Author

**Maintainer**: Pieter Riesenkamp <p.b.riesenkamp@student.rug.nl>
