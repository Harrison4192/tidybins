
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quantileR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/quantileR)](https://CRAN.R-project.org/package=quantileR)
[![R-CMD-check](https://github.com/Harrison4192/quantileR/workflows/R-CMD-check/badge.svg)](https://github.com/Harrison4192/quantileR/actions)
[![](http://cranlogs.r-pkg.org/badges/grand-total/dataCleaner?color=blue)](https://cran.r-project.org/package=dataCleaner)
[![](https://img.shields.io/github/languages/code-size/Harrison4192/dataCleaner.svg)](https://github.com/Harrison4192/dataCleaner)
[![](https://img.shields.io/github/last-commit/Harrison4192/dataCleaner.svg)](https://github.com/Harrison4192/dataCleaner/commits/master)
<!-- badges: end -->

The goal of quantileR is to …

## Installation

You can install the released version of quantileR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("quantileR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(quantileR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
