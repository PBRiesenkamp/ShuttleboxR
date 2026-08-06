# ShuttleboxR: Data Preparation and Exploration for Shuttle-Box Temperature Experiments

ShuttleboxR bridges the gap between data acquisition and statistical
analysis in shuttle-box temperature experiments. It imports and
organises recordings, calculates reproducible thermal and behavioural
metrics, and provides plots and project-level screens for inspecting
data quality.

## Workflow

The package follows three consecutive stages:

1.  **Import and organise** the data.

2.  **Calculate shuttle-box metrics** using explicit settings.

3.  **Inspect and troubleshoot** the data before formal analysis.

The stages are applied to two connected branches. The single-trial
branch calculates and inspects one fish. The project branch compiles one
row of metrics per fish, examines distributions and multivariate
patterns across the study, and flags trials that should be returned to
the single-trial plots for closer review.

## Interpretation

Project-level outlier screens identify candidates for inspection, not
automatic exclusions. Unusual values should be checked against the
original temperature, tracking and movement records and interpreted in
the context of the species and experimental design. ShuttleboxR prepares
and explores data; it does not choose the inferential analysis for a
study.

## Core temperature

ShuttleSoft files normally already include `core_T`. Recalculation with
[`calc_coreT()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_coreT.md)
is optional and requires calibrated thermal-lag parameters.

## Gravitation-aware metrics

The transition from starting conditions to settled thermal behaviour can
be estimated with
[`calc_gravitation()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/calc_gravitation.md)
and inspected with
[`plot_T_segmented()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/plot_T_segmented.md).
When requested, package functions exclude this period automatically from
Tpref, avoidance temperatures, Tbreadth, percentile-based thermal range,
core-temperature variation, limit exposure, and activity metrics.

## Getting started

Run
[`vignette("ShuttleboxR", package = "ShuttleboxR")`](https://pbriesenkamp.github.io/ShuttleboxR/articles/ShuttleboxR.md)
for a worked example covering both the single-trial and project-level
workflows.

## Author

**Maintainer**: Pieter Riesenkamp <p.b.riesenkamp@student.rug.nl>
