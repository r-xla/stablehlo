# BroadcastInDim Operator

See <https://openxla.org/stablehlo/spec#broadcast_in_dim> for details.

## Usage

``` r
infer_types_broadcast_in_dim(operand, broadcast_dimensions, shape)

hlo_broadcast_in_dim(operand, broadcast_dimensions, shape, output_types = NULL)
```

## Arguments

- operand, broadcast_dimensions, shape:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

- output_types:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [`ValueType`](https://r-xla.github.io/stablehlo/dev/reference/ValueType.md)
  \| `NULL`)  
  Output types known ahead of time (e.g. from type inference at trace
  time). When provided, type inference and its input validation are
  skipped.

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
