# CountLeadingZeros Operator

See <https://openxla.org/stablehlo/spec#count_leading_zeros> for
details.

## Usage

``` r
infer_types_count_leading_zeros(operand)

hlo_count_leading_zeros(operand, output_types = NULL)
```

## Arguments

- operand:

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
