# Gather Operator

See <https://openxla.org/stablehlo/spec#gather> for details.

## Usage

``` r
infer_types_gather(
  operand,
  start_indices,
  gather_dimension_numbers,
  slice_sizes,
  indices_are_sorted
)

hlo_gather(
  operand,
  start_indices,
  gather_dimension_numbers,
  slice_sizes,
  indices_are_sorted = FALSE
)
```

## Arguments

- operand, start_indices:

  ([`FuncValue`](FuncValue.md))  

- gather_dimension_numbers:

  (`GatherDimensionNumbers`)  
  The gather dimension numbers.

- slice_sizes:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The sizes of the slices to gather.

- indices_are_sorted:

  (`logical(1)`)  
  Whether indices are sorted.

## Value

[`FuncValue`](FuncValue.md)  
