
<!-- README.md is generated from README.Rmd. Please edit that file -->

# latex2r

The goal of latex2r is to translate latex formulas to R code.

THE PACKAGE IS UNDER VERY EARLY DEVELOPMENT, DO NOT USE\!

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tomicapretto/latex2r")
```

## Example

``` r
library(latex2r)
latex2r("\\beta_1^{\\frac{x+1}{x^2 \\cdot y}}")
#> [1] "beta_1^((x + 1) / (x^2 * y))"
```
