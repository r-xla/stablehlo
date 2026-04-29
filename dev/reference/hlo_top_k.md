# TopK Operator (CHLO)

This op is from the CHLO dialect, a higher-level companion to stableHLO
that is lowered to stableHLO during compilation. See
<https://openxla.org/stablehlo/generated/chlo#chlotop_k_chlotop_kop> for
details.

## Usage

``` r
infer_types_top_k(operand, k)

hlo_top_k(operand, k)
```

## Arguments

- operand:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  
  Tensor of integer, unsigned integer, or floating-point type with rank
  \>= 1.

- k:

  (`integer(1)`)  
  Number of top elements to return along the last dimension. Must
  satisfy `1 <= k <= dim(operand, -1)`.

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  

A [`list()`](https://rdrr.io/r/base/list.html) of two
[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)s:
the top-k values (same dtype as `operand`) and their indices into the
last dimension (dtype `i32`). Ties are broken by lower index first.
