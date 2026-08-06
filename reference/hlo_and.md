# And Operator

See <https://openxla.org/stablehlo/spec#and> for details.

## Usage

``` r
infer_types_and(lhs, rhs)

hlo_and(lhs, rhs, output_types = NULL)
```

## Arguments

- lhs, rhs:

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
