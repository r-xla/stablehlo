# Sinh Operator (CHLO)

This op is from the CHLO dialect, a higher-level companion to stableHLO
that is lowered to stableHLO during compilation. See
<https://openxla.org/stablehlo/generated/chlo#chlosinh_chlosinhop> for
details.

## Usage

``` r
infer_types_sinh(operand)

hlo_sinh(operand, output_types = NULL)
```

## Arguments

- operand:

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
