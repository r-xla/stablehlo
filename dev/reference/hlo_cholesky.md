# Cholesky Operator

See <https://openxla.org/stablehlo/spec#cholesky> for details.

## Usage

``` r
hlo_cholesky(operand, lower, output_types = NULL)
```

## Arguments

- operand, lower:

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

## Details

The values of the other half of the matrix are not guaranteed and
backend dependent.
