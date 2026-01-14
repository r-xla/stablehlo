# Clamp Operator

See <https://openxla.org/stablehlo/spec#clamp> for details.

## Usage

``` r
infer_types_clamp(min, operand, max)

hlo_clamp(min, operand, max)
```

## Arguments

- min, operand, max:

  ([`FuncValue`](FuncValue.md))  

## Value

[`FuncValue`](FuncValue.md)  
