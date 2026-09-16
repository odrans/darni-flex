# darflex

`darflex` provides a set of tools for handling dardar and flexpart data

This is not adapted for public use at the moment (input files are not all public)

- `load_data` Create a DARNI-origin file based on DARDAR and FLEXPART inputs

## Installation

darflex is a [R](https://www.r-project.org/) package. It is not available on CRAN but can be installed by using the `devtools` package:

```R
devtools::install_github("odrans/darflex")
```

## Scripts and analysis

These folders are not part of the package build. Run them from the repository root.

- `scripts/darni_flex_run.r` creates DARni-origin files from DARDAR-Nice and FLEXPART files with
  `load_data()`.
- `analysis/darni_flex.r` loads the DARni-origin files and draws the cloud-origin figures defined in
  `analysis/fig_*.r`. It sources the package code from `R/`, except the origin loaders, which come from
  `analysis/load_dardar_origin.r`: that copy's 3D loader writes RDS files instead of returning a data
  frame, and drops `origin_flag` and rows without `dt_cloud`.

Data locations follow the `~/storage` layout that chezmoi creates on each host; `DARNI_WORK` and
`DARNI_SCRATCH` override them, as in `darni-rad`.
