# Pad Operator

See <https://openxla.org/stablehlo/spec#pad> for details.

## Usage

``` r
infer_types_pad(
  operand,
  padding_value,
  edge_padding_low,
  edge_padding_high,
  interior_padding
)

hlo_pad(
  operand,
  padding_value,
  edge_padding_low,
  edge_padding_high,
  interior_padding
)
```

## Arguments

- operand, padding_value, edge_padding_low, edge_padding_high,
  interior_padding:

  (`FuncVariable`)  

## Value

`FuncVariable`  
