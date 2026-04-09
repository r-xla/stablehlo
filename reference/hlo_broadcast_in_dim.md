# BroadcastInDim Operator

See <https://openxla.org/stablehlo/spec#broadcast_in_dim> for details.

## Usage

``` r
infer_types_broadcast_in_dim(operand, broadcast_dimensions, shape)

hlo_broadcast_in_dim(operand, broadcast_dimensions, shape)
```

## Arguments

- operand, broadcast_dimensions, shape:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/reference/FuncValue.md))  

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/reference/FuncValue.md)  
