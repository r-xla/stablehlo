# DynamicSlice Operator

See <https://openxla.org/stablehlo/spec#dynamic_slice> for details.

## Usage

``` r
infer_types_dynamic_slice(operand, ..., slice_sizes)

hlo_dynamic_slice(operand, ..., slice_sizes)
```

## Arguments

- operand, ..., slice_sizes:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
