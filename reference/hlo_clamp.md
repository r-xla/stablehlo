# Clamp Operator

See <https://openxla.org/stablehlo/spec#clamp> for details.

## Usage

``` r
infer_types_clamp(Min, operand, Max)

hlo_clamp(Min, operand, Max)
```

## Arguments

- Min, operand, Max:

  ([`FuncVariable`](FuncVariable.md))  

## Value

[`FuncVariable`](FuncVariable.md)  
