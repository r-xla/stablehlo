
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stablehlo

<!-- badges: start -->

![R-CMD-check](https://github.com/r-xla/stablehlo/actions/workflows/R-CMD-check.yaml/badge.svg)
![work-in-progress](https://img.shields.io/badge/status-work%20in%20progress-yellow)
<!-- badges: end -->

The {stablehlo} R package provides a functional API to creating
[stableHLO](https://github.com/openxla/stablehlo) programs. The thereby
created programs can be executed using the R package
[pjrt](https://github.com/r-xla/pjrt).

## Installation

You can install the development version of stablehlo from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("r-xla/stablehlo")
```

## What can it do?

Using a functional API, we can build up programs using the `hlo_<op>`
functions, which is the API through which the package should be used.

To create a new function with a single argument, we can use the
`hlo_input` function and specify its type as well as the function id.
Below, we create two function inputs.

``` r
library(stablehlo)
pp <- function(x) cat(repr(x@func), "\n")
# we only need to specify the func_id for one of the inputs
x <- hlo_input("x", "f32", shape = c(2, 2), func_id = "main")
pp(x)
#> func.func @main (%x: tensor<2x2xf32>) ->  {
#> 
#> }
y <- hlo_input("y", "f32", shape = c(2, 2))
pp(y)
#> func.func @ (%y: tensor<2x2xf32>) ->  {
#> 
#> }
```

To create a function that has these `x` and `y` as arguments and adds
them, we can pass the two variables to the `hlo_add()` function:

``` r
z <- hlo_add(x, y)
pp(z)
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> %1 ="stablehlo.add"(%x, %y):(tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
#> }
```

We can continue building up the function, e.g. by calculating the
absolute value of the result.

``` r
z_abs <- hlo_abs(z)
pp(z_abs)
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> %1 ="stablehlo.add"(%x, %y):(tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
#> %2 ="stablehlo.abs"(%1):(tensor<2x2xf32>) -> (tensor<2x2xf32>)
#> }
```

Once we are done defining the function arguments and body, we use the
`hlo_return` function.

``` r
f <- hlo_return(z, z_abs)
f
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) -> tensor<2x2xf32>, tensor<2x2xf32> {
#> %1 ="stablehlo.add"(%x, %y):(tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
#> %2 ="stablehlo.abs"(%1):(tensor<2x2xf32>) -> (tensor<2x2xf32>)
#> "stablehlo.return"(%1, %2):(tensor<2x2xf32>, tensor<2x2xf32>) -> ()
#> }
```

## How does it work?

The central class the underpins the API is the `FuncPointer` class. It
has three fields, namely the name and type of the variable it “points
to” and the function it belongs to.

For example, for the `x` variable above, we have

``` r
repr(x@value_id)
#> [1] "%x"
repr(x@value_type)
#> [1] "tensor<2x2xf32>"
repr(x@func)
#> [1] "func.func @main (%x: tensor<2x2xf32>) ->  {\n\n}"
```

When we combine two `FuncPointer`s, we:

1.  Merge the underlying functions by combining their arguments and
    body:
2.  Add the new operation to the bottom of the body
3.  Create a (list of) `FuncPointer`(s) that represent the outputs of
    the applied operation.

Note that all variable names but the the argument names are considered
an internal implementation detail.

The `hlo_return()` function is special, because it does not return a
`FuncPointer` but instead the function itself. This is, because after
returning, a function can no longer be modified.

## What it can’t do?

Initially, we will:

- only support a subset of the available operations, see [this
  issue](https://github.com/r-xla/stablehlo/issues/6) for the currently
  supported operations.
- not support quantization
- not support complex numbers

## Contributing

The easiest way to contribute is to implement a new operator. See [this
issue](https://github.com/r-xla/stablehlo/issues/6) for those that are
already implemented. The definition of the primitive stableHLO operators
can be found [here](https://openxla.org/stablehlo/spec#ops).

To implement a new primitive, you need to create:

1.  An `Op` class that represents the operator.
2.  Implement the type inference for the operator, i.e. for which inputs
    it produces which outputs.
3.  Implement the API function (`hlo_<opname>`) for the operation.
4.  (TODO) Add the tests for the operation. The testing infrastructure
    is not yet in place, see [this
    issue](https://github.com/r-xla/stablehlo/issues/9).
