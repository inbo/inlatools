# The `inlatools` package

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.4.0-6666ff.svg)](https://cran.r-project.org/)
[![wercker status](https://app.wercker.com/status/d0205ffb719d28fac3f8ebe492a0caef/s/master "wercker status")](https://app.wercker.com/project/byKey/d0205ffb719d28fac3f8ebe492a0caef)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/inbo/inlatools?branch=master&svg=true)](https://ci.appveyor.com/project/inbo/inlatools)
[![codecov](https://codecov.io/gh/inbo/inlatools/branch/master/graph/badge.svg)](https://codecov.io/gh/inbo/inlatools)

## Rationale

The `inlatools` package provides a set of function which can be useful to diagnose [INLA](http://www.r-inla.org/) models.

- calculate Pearson residuals
- simulation based checks for over- or underdispersion
- simulation based checks for the distribution
- visualise the effect of the variance or precision on random effects
    - random intercept
    - first order random walk
    - second order random walk

## Installation

```r
ip <- rownames(installed.packages())
if (!"remotes" %in% ip) {
  install.packages("remotes")
}
if (!"INLA" %in% ip) {
  install.packages(
    "INLA", 
    repos = c(getOption("repos"), "https://inla.r-inla-download.org/R/stable")
  )
}
remotes::install_github("inbo/inlatools")
```

## Documentation

All documentation is available at the package website: https://inlatools.netlify.com. This website contains an [overview](https://inlatools.netlify.com/reference/index.html) of all available functions. Click on the name the function to open the corresponding help file. The [vignettes](https://inlatools.netlify.com/articles/index.html) illustrate how to [check the dispersion](https://inlatools.netlify.com/articles/dispersion.html), how to [check the distribution](https://inlatools.netlify.com/articles/distribution.html) and how to visualise [random effects](https://inlatools.netlify.com/articles/prior.html) for a given variance or precision.

## Folder structure

- `R`: The source scripts of the [R](https://cloud.r-project.org/) functions with documentation in [Roxygen](https://github.com/klutometis/roxygen) format
- `man`: The helpfile in [Rd](https://cloud.r-project.org/doc/manuals/r-release/R-exts.html#Rd-format) format
- `test`: unit tests using the [`testthat`](http://testthat.r-lib.org/) framework
- `vignettes`: the source code of the vignettes

```
inlatools
├── man
├── R
├─┬ tests
│ └── testthat
└── vignettes
```
