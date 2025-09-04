
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stablehlo

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
![R-CMD-check](https://github.com/r-xla/stablehlo/actions/workflows/R-CMD-check.yaml/badge.svg)
[![CRAN
status](https://www.r-pkg.org/badges/version/stablehlo)](https://CRAN.R-project.org/package=stablehlo)
[![codecov](https://codecov.io/gh/r-xla/stablehlo/branch/main/graph/badge.svg)](https://codecov.io/gh/r-xla/stablehlo)
<!-- badges: end -->

The {stablehlo} R package provides a functional API to create
[stableHLO](https://openxla.org/stablehlo) programs. These programs can
be executed using the R package [pjrt](https://github.com/r-xla/pjrt).

## Installation

``` r
pak::pak("r-xla/stablehlo")
```

## Quickstart

Below, we create a function that takes two input arguments `x` and `y`
of type `f32` and shape `(2, 2)` and adds them.

``` r
library(stablehlo)
x <- hlo_input("x", "f32", shape = c(2, 2), func_id = "main")
x
#> Variable %x in:
#> func.func @main (%x: tensor<2x2xf32>) ->  {
#> 
#> }
y <- hlo_input("y", "f32", shape = c(2, 2), func_id = "main")
y
#> Variable %y in:
#> func.func @main (%y: tensor<2x2xf32>) ->  {
#> 
#> }
z <- hlo_add(x, y)
z
#> Variable %1 in:
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> %1 = "stablehlo.add" (%x, %y): (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
#> }
f <- hlo_return(z)
f
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) -> tensor<2x2xf32> {
#> %1 = "stablehlo.add" (%x, %y): (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
#> "func.return"(%1): (tensor<2x2xf32>) -> ()
#> }
```

## Restrictions

The R package should be considered a *partial* implementation of the
stableHLO specification. At least initially, it will:

- only support a subset of the available operations, see [this
  issue](https://github.com/r-xla/stablehlo/issues/6) for an overview.
- not support all datatypes, e.g. quantized types and complex numbers
  are not supported.
