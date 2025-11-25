# Slice Operator

See <https://openxla.org/stablehlo/spec#slice> for details.

## Usage

``` r
infer_types_slice(operand, start_indices, limit_indices, strides)

hlo_slice(operand, start_indices, limit_indices, strides)
```

## Arguments

- operand, start_indices, limit_indices, strides:

  ([`FuncVariable`](FuncVariable.md))  

## Value

[`FuncVariable`](FuncVariable.md)  
