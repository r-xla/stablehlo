
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stablehlo

Package website: [release](https://r-xla.github.io/stablehlo/) \|
[dev](https://r-xla.github.io/stablehlo/dev/)

<!-- badges: start -->

![R-CMD-check](https://github.com/r-xla/stablehlo/actions/workflows/R-CMD-check.yaml/badge.svg)
[![CRAN
status](https://www.r-pkg.org/badges/version/stablehlo)](https://CRAN.R-project.org/package=stablehlo)
[![codecov](https://codecov.io/gh/r-xla/stablehlo/branch/main/graph/badge.svg)](https://codecov.io/gh/r-xla/stablehlo)
[![r-universe](https://r-xla.r-universe.dev/badges/stablehlo)](https://r-xla.r-universe.dev/stablehlo)
<!-- badges: end -->

The {stablehlo} R package provides a functional API to create
[stableHLO](https://openxla.org/stablehlo) programs. It also supports
selected ops from the
[CHLO](https://openxla.org/stablehlo/generated/chlo) dialect, a
higher-level companion to stableHLO that is lowered to stableHLO during
compilation. These programs can be executed using the R package
[pjrt](https://github.com/r-xla/pjrt).

{stablehlo} is the IR layer of the [r-xla](https://github.com/r-xla)
ecosystem. The main user-facing package is
[{anvl}](https://github.com/r-xla/anvl), a code transformation framework
for R that builds on {stablehlo} and {pjrt} to provide JIT compilation
and automatic differentiation. Most users should reach for {anvl}
directly; {stablehlo} is intended for those who want to construct or
manipulate stableHLO programs at the IR level.

## Installation

From GitHub:

``` r
pak::pak("r-xla/stablehlo")
```

You can also install from
[r-universe](https://r-xla.r-universe.dev/builds), by adding the code
below to your `.Rprofile`.

``` r
options(repos = c(
  rxla = "https://r-xla.r-universe.dev",
  CRAN = "https://cloud.r-project.org/"
))
```

## Quickstart

Below, we create a function that takes two input arguments `x` and `y`
of type `f32` and shape `2x2` and adds them. Passing `func` to
`hlo_input()` is optional, because it will automatically use the last
function created with `hlo_func()`.

``` r
library(stablehlo)
func <- hlo_func("main")
func
#> func.func @main () ->  {
#> 
#> }
x <- hlo_input("x", "f32", shape = c(2, 2), func = func)
x
#> Variable %x in:
#> func.func @main (%x: tensor<2x2xf32>) ->  {
#> 
#> }
y <- hlo_input("y", "f32", shape = c(2, 2), func = func)
y
#> Variable %y in:
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> 
#> }
z <- hlo_add(x, y)
z
#> Variable %0 in:
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> %0 = stablehlo.add %x, %y : tensor<2x2xf32>
#> }
f <- hlo_return(z)
identical(f, func)
#> [1] TRUE
f
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) -> tensor<2x2xf32> {
#> %0 = stablehlo.add %x, %y : tensor<2x2xf32>
#> return %0 : tensor<2x2xf32>
#> }
```

## Main Features

- Implements type inference to easily build up stableHLO programs.
- Simple, functional API.
- Clear error messages.
- Easy installation because the implementation is in R and does not
  depend on the stableHLO C++ builder, which depends on LLVM and MLIR.
- Supports a subset of
  [CHLO](https://openxla.org/stablehlo/generated/chlo) ops (e.g. inverse
  trig, hyperbolic, gamma family) that are not StableHLO primitives and
  would otherwise have to be hand-composed.

## Important notes

- stableHLO uses 0-based indexing. Wherever operations take dimension
  indices (e.g., axes, start indices, permutation dimensions), use
  0-based values. This differs from R’s 1-based indexing.
- We try to use `"bool"` for the `BooleanType`. However, in the
  generated stableHLO representation this will show up as `"i1"`. Think
  of these as interchangeable.

## Limitations

The R package is a *partial* implementation of the stableHLO
specification. At least initially, it will:

- only support a subset of the available operations, see [this
  issue](https://github.com/r-xla/stablehlo/issues/6) for an overview.
- not support all data types, e.g. quantized types and complex numbers
  are not supported.
- not support shape dynamism.

## Acknowledgments

- This work is supported by [MaRDI](https://www.mardi4nfdi.de).
- This work is built upon the [stableHLO
  specification](https://openxla.org/stablehlo/spec) from the OpenXLA
  project.
