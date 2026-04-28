# Polygamma Operator (CHLO)

This op is from the CHLO dialect, a higher-level companion to stableHLO
that is lowered to stableHLO during compilation. See
<https://openxla.org/stablehlo/generated/chlo#chlopolygamma_chlopolygammaop>
for details.

## Usage

``` r
infer_types_polygamma(n, x)

hlo_polygamma(n, x)
```

## Arguments

- n, x:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
