
<!-- README.md is generated from README.Rmd. Please edit that file -->

# framerrr

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg)](https://choosealicense.com/licenses/mit/)
[![Travis build
status](https://travis-ci.com/mickmioduszewski/framerrr.svg?branch=master)](https://travis-ci.com/mickmioduszewski/framerrr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mickmioduszewski/framerrr?branch=master&svg=true)](https://ci.appveyor.com/project/mickmioduszewski/framerrr)
[![Codecov test
coverage](https://codecov.io/gh/mickmioduszewski/framerrr/branch/master/graph/badge.svg)](https://codecov.io/gh/mickmioduszewski/framerrr?branch=master)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/framerrr)](https://cran.r-project.org/package=framerrr)
[![CRAN\_latest\_release\_date](https://www.r-pkg.org/badges/last-release/framerrr)](https://cran.r-project.org/package=framerrr)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/grand-total/framerrr)](https://cran.r-project.org/package=framerrr)
![](https://img.shields.io/github/languages/top/mickmioduszewski/framerrr.svg)
![](https://img.shields.io/github/issues/mickmioduszewski/framerrr.svg)
<!-- badges: end -->

The goal of framerrr is to easier to prevent bugs when porting between
environments by separating environment dependent information in
configuration files. Files paths are logical and not physically hard
coded. Provides helper functions easing common problems in
cross-environment software.

## Installation

You can install the released version of framerrr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("framerrr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mickmioduszewski/framerrr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(framerrr)
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
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
