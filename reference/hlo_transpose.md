# Transpose Operator

See <https://openxla.org/stablehlo/spec#transpose> for details.

## Usage

``` r
infer_types_transpose(operand, permutation)

hlo_transpose(operand, permutation)
```

## Arguments

- operand, permutation:

  ([`FuncVariable`](FuncVariable.md))  

## Value

[`FuncVariable`](FuncVariable.md)  
