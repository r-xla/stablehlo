# Transpose Operator

See <https://openxla.org/stablehlo/spec#transpose> for details.

## Usage

``` r
infer_types_transpose(operand, permutation)

hlo_transpose(operand, permutation)
```

## Arguments

- operand, permutation:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
