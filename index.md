# stablehlo

The {stablehlo} R package provides a functional API to create
[stableHLO](https://openxla.org/stablehlo) programs. These programs can
be executed using the R package [pjrt](https://github.com/r-xla/pjrt).

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
of type `f32` and shape `(2, 2)` and adds them. Passing `func` to
[`hlo_input()`](reference/hlo_input.md) is optional, because it will
automatically use the last function created with
[`hlo_func()`](reference/hlo_func.md).

``` r
library(stablehlo)
func <- hlo_func("myfn")
func
#> func.func @myfn () ->  {
#> 
#> }
x <- hlo_input("x", "f32", shape = c(2, 2), func = func)
x
#> Variable %x in:
#> func.func @myfn (%x: tensor<2x2xf32>) ->  {
#> 
#> }
y <- hlo_input("y", "f32", shape = c(2, 2), func = func)
y
#> Variable %y in:
#> func.func @myfn (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> 
#> }
z <- hlo_add(x, y)
z
#> Variable %0 in:
#> func.func @myfn (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> %0 = "stablehlo.add" (%x, %y): (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
#> }
f <- hlo_return(z)
identical(f, func)
#> [1] TRUE
f
#> func.func @myfn (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) -> tensor<2x2xf32> {
#> %0 = "stablehlo.add" (%x, %y): (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
#> "func.return"(%0): (tensor<2x2xf32>) -> ()
#> }
```

## Important notes

stableHLO uses 0-based indexing. Wherever operations take dimension
indices (e.g., axes, start indices, permutation dimensions), use 0-based
values. This differs from R’s 1-based indexing.

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
