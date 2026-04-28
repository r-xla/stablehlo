# Lgamma Operator (CHLO)

This op is from the CHLO dialect, a higher-level companion to stableHLO
that is lowered to stableHLO during compilation. See
<https://openxla.org/stablehlo/generated/chlo#chlolgamma_chlolgammaop>
for details.

## Usage

``` r
infer_types_lgamma(operand)

hlo_lgamma(operand)
```

## Arguments

- operand:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
