# Maintaining ShuttleboxR

## Thermal spread metrics (0.0.0.9015)

`Tpref_range`, `Tpercentile_range`, and `Tbreadth` are deliberately
distinct. `Tpref_range` uses the avoidance percentiles,
`Tpercentile_range` uses user-selected percentile boundaries (25th and
75th by default), and `Tbreadth` is the mean pairwise temperature
difference across the complete distribution. Keep these distinctions
explicit in documentation and analyses.

## Documentation style (0.0.0.9014)

User-facing examples should show the shortest normal call first. Do not
expose internal helper objects, repeated breakpoint fitting,
[`tryCatch()`](https://rdrr.io/r/base/conditions.html),
[`do.call()`](https://rdrr.io/r/base/do.call.html), or manual
result-printing code in the README or vignette. Advanced overrides may
be described after the basic workflow.

## Gravitation-aware windows (0.0.0.9012)

Thermal distribution metrics can now exclude the transition before the
segmented breakpoint. The gravitation duration is referenced to either
the dynamic-period start or the recording start, controlled by
`exclude_acclimation`. Project calculations estimate gravitation once
per fish and reuse the same value across dependent metrics.

This file is a short checklist for making and publishing package
changes.

## Edit and test a function

1.  Open `ShuttleboxR.Rproj` in RStudio.
2.  Edit the relevant file inside the `R` folder.
3.  Load the edited package:

``` r

devtools::load_all()
```

4.  Try the function on an example file.

## Update help pages

The help-page text is written in the lines beginning with `#'` above
each function. After editing those lines, run:

``` r

devtools::document()
```

Do not edit files in `man` directly; they are generated from the
comments in `R`.

## Preview the vignette

``` r

rmarkdown::render("vignettes/ShuttleboxR.Rmd")
```

This creates `vignettes/ShuttleboxR.html` in the project folder.

To install the package with the vignette included:

``` r

devtools::install(build_vignettes = TRUE)
```

Then open it with:

``` r

browseVignettes("ShuttleboxR")
```

## Check the complete package

``` r

devtools::document()
devtools::check()
```

Aim for no errors, warnings, or notes before publishing an update.

## Build the documentation website locally

``` r

install.packages("pkgdown")
pkgdown::build_site()
```

The GitHub workflow in `.github/workflows/pkgdown.yaml` can build and
publish the site automatically after the repository settings for GitHub
Pages have been enabled.

## Upload changes with GitHub Desktop

1.  Review the changed files in GitHub Desktop.
2.  Enter a brief summary of the update.
3.  Click **Commit to master**.
4.  Click **Push origin**.

## PCA reproducibility

Always supply the `variables` argument when using
[`pca()`](https://pbriesenkamp.github.io/ShuttleboxR/reference/pca.md)
in examples or analyses. This prevents a future numeric column from
changing the PCA without the user noticing. Report `mahalanobis_th`,
`dbscan_th`, `dbscan_minPts`, and `flag_rule` alongside the selected
variables.

## Plotting convention

All ggplot-based package figures use
[`ggplot2::theme_classic()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
so figures have a consistent appearance without gridlines. Apply the
same theme to any new plot functions and to plots returned by helper
packages such as factoextra.

## Bundled examples

The current single-trial example is `inst/extdata/Fish_14_13_2.txt`.
When the example changes, update the filename in the README, vignette,
roxygen examples, and generated help pages, and verify that the vignette
still renders from a clean package installation.
