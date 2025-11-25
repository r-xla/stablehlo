# Create a Constant

Create either a "scalar" (`hlo_scalar`) or tensor (`hlo_tensor`)
constant. Strictly speaking, stableHLO "scalars" are simply tensors with
0 dimensions. To create an empty constant (at least one dimension is 0),
use `hlo_empty`.

## Usage

``` r
hlo_scalar(value, ..., func = NULL)

hlo_tensor(value, ..., func = NULL)

hlo_empty(dtype, shape, func = NULL)
```

## Arguments

- value:

  (any)  
  Value from which to create a constant.

- ...:

  (any)  
  Additional arguments.

- func:

  ([`Func`](Func.md))  
  The function to add the constant to. Per default, uses the last
  function created with [`hlo_func`](hlo_func.md) or
  [`local_func`](hlo_func.md).

- dtype:

  (`character(1)`)  
  One of: pred, i8, i16, i32, i64, ui8, ui16, ui32, ui64, f32, f64.

- shape:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The shape

## Examples

``` r
hlo_scalar(1L, dtype = "i32", func = Func())
#> Variable %0 in:
#> func.func @ () ->  {
#> %0 = "stablehlo.constant" () {
#> value = dense<1> : tensor<i32>
#> }: () -> (tensor<i32>)
#> }
hlo_scalar(1, dtype = "f32", func = Func())
#> Variable %0 in:
#> func.func @ () ->  {
#> %0 = "stablehlo.constant" () {
#> value = dense<1.00000000e+00> : tensor<f32>
#> }: () -> (tensor<f32>)
#> }
hlo_scalar(TRUE, func = Func())
#> Variable %0 in:
#> func.func @ () ->  {
#> %0 = "stablehlo.constant" () {
#> value = dense<true> : tensor<i1>
#> }: () -> (tensor<i1>)
#> }
hlo_tensor(array(c(1, 2, 3, 4), dim = c(1, 4)), dtype = "f32", func = Func())
#> Variable %0 in:
#> func.func @ () ->  {
#> %0 = "stablehlo.constant" () {
#> value = dense<[[1.00000000e+00, 2.00000000e+00, 3.00000000e+00, 4.00000000e+00]]> : tensor<1x4xf32>
#> }: () -> (tensor<1x4xf32>)
#> }
hlo_empty(dtype = "f32", shape = c(0, 3), func = Func())
#> Variable %0 in:
#> func.func @ () ->  {
#> %0 = "stablehlo.constant" () {
#> value = dense<> : tensor<0x3xf32>
#> }: () -> (tensor<0x3xf32>)
#> }
```
