# Slice Operator

See <https://openxla.org/stablehlo/spec#slice> for details.

## Usage

``` r
infer_types_slice(operand, start_indices, limit_indices, strides)

hlo_slice(operand, start_indices, limit_indices, strides, output_types = NULL)
```

## Arguments

- operand, start_indices, limit_indices, strides:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/reference/FuncValue.md))  

- output_types:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [`ValueType`](https://r-xla.github.io/stablehlo/reference/ValueType.md)
  \| `NULL`)  
  Output types known ahead of time (e.g. from type inference at trace
  time). When provided, type inference and its input validation are
  skipped.

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/reference/FuncValue.md)  
