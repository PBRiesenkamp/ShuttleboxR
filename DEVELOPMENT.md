# Maintaining ShuttleboxR

This file is a short checklist for making and publishing package changes.

## Edit and test a function

1. Open `ShuttleboxR.Rproj` in RStudio.
2. Edit the relevant file inside the `R` folder.
3. Load the edited package:

```r
devtools::load_all()
```

4. Try the function on an example file.

## Update help pages

The help-page text is written in the lines beginning with `#'` above each
function. After editing those lines, run:

```r
devtools::document()
```

Do not edit files in `man` directly; they are generated from the comments in
`R`.

## Preview the vignette

```r
devtools::build_rmd("vignettes/ShuttleboxR.Rmd")
```

To install the package with the vignette included:

```r
devtools::install(build_vignettes = TRUE)
```

Then open it with:

```r
browseVignettes("ShuttleboxR")
```

## Check the complete package

```r
devtools::document()
devtools::check()
```

Aim for no errors, warnings, or notes before publishing an update.

## Build the documentation website locally

```r
install.packages("pkgdown")
pkgdown::build_site()
```

The GitHub workflow in `.github/workflows/pkgdown.yaml` can build and publish the
site automatically after the repository settings for GitHub Pages have been
enabled.

## Upload changes with GitHub Desktop

1. Review the changed files in GitHub Desktop.
2. Enter a brief summary of the update.
3. Click **Commit to master**.
4. Click **Push origin**.
