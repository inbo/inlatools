# The `inlatools` package

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.4.0-6666ff.svg)](https://cran.r-project.org/)
[![Travis-CI Build Status](https://travis-ci.org/inbo/inlatools.svg?branch=master)](https://travis-ci.org/inbo/inlatools)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/inbo/inlatools?branch=master&svg=true)](https://ci.appveyor.com/project/inbo/inlatools)

## Rationale

The `inlatools` package provides a set of function which can be useful to diagnose [INLA](http://www.r-inla.org/) models.

- calculate Pearson residuals
- simulation based test for over- or underdispersion

## Installation

```
ip <- rownames(installed.packages())
if (!"devtools" %in% ip) {
  install.packages("devtools")
}
if (!"INLA" %in% ip) {
  install.packages(
    "INLA", 
    repos = c(getOption("repos"), "https://inla.r-inla-download.org/R/stable")
  )
}
devtools::install_github("inbo/inlatools")
```

## Folder structure

- `R`: The source scripts of the [R](https://cloud.r-project.org/) functions with documentation in [Roxygen](https://github.com/klutometis/roxygen) format
- `man`: The helpfile in [Rd](https://cloud.r-project.org/doc/manuals/r-release/R-exts.html#Rd-format) format

```
inlatools
├── man
└── R
```
