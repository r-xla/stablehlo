# DynamicUpdateSlice Operator

See <https://openxla.org/stablehlo/spec#dynamic_update_slice> for
details.

## Usage

``` r
infer_types_dynamic_update_slice(operand, update, ...)

hlo_dynamic_update_slice(operand, update, ...)
```

## Arguments

- operand, update, ...:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
