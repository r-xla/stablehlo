# DynamicSlice Operator

See <https://openxla.org/stablehlo/spec#dynamic_slice> for details.

## Usage

``` r
infer_types_dynamic_slice(operand, ..., slice_sizes)

hlo_dynamic_slice(operand, ..., slice_sizes, output_types = NULL)
```

## Arguments

- operand, ..., slice_sizes:

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
