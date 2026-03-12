# Slice Operator

See <https://openxla.org/stablehlo/spec#slice> for details.

## Usage

``` r
infer_types_slice(operand, start_indices, limit_indices, strides)

hlo_slice(operand, start_indices, limit_indices, strides)
```

## Arguments

- operand, start_indices, limit_indices, strides:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
