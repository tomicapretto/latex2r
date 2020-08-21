
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

For those who dare to go a little further

``` r
eval(parse(text = latex2r::latex2r("\\tanh(x_1 ^ 2)")), envir = list(x_1 = 2))
#> [1] 0.9993293
```

For those who are crazy enough to navigate the deepest waters of R, you
can see the [source code](R/latex2fun.R) of `latex2fun`.

NB: The following is very experimental, but shows the power of what you
can do with this package (and with R).

``` r
x = seq(-2*pi, 2*pi, length.out = 500)
f = latex2fun("\\sin{a * x}^2 + \\cos{b * x} ^2")
print(f)
#> function (a, b, x) 
#> sin(a * x)^2 + cos(b * x)^2

y = f(x = x, a = 2, b = 3)
par(mar = c(2, 2, 2, 2))
plot(x, y, type = "l")
```

<img src="man/figures/README-example3-1.png" width="75%" style="display: block; margin: auto;" />
