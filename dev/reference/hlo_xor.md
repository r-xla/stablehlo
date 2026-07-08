# Xor Operator

See <https://openxla.org/stablehlo/spec#xor> for details.

## Usage

``` r
infer_types_xor(lhs, rhs)

hlo_xor(lhs, rhs, output_types = NULL)
```

## Arguments

- lhs, rhs:

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
