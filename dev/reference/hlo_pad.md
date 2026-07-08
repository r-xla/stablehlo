# Pad Operator

See <https://openxla.org/stablehlo/spec#pad> for details.

## Usage

``` r
infer_types_pad(
  operand,
  padding_value,
  edge_padding_low,
  edge_padding_high,
  interior_padding
)

hlo_pad(
  operand,
  padding_value,
  edge_padding_low,
  edge_padding_high,
  interior_padding,
  output_types = NULL
)
```

## Arguments

- operand, padding_value, edge_padding_low, edge_padding_high,
  interior_padding:

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
