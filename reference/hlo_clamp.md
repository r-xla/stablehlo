# Clamp Operator

See <https://openxla.org/stablehlo/spec#clamp> for details.

## Usage

``` r
infer_types_clamp(Min, operand, Max)

hlo_clamp(Min, operand, Max)
```

## Arguments

- Min, operand, Max:

  ([`FuncValue`](FuncValue.md))  

## Value

[`FuncValue`](FuncValue.md)  
