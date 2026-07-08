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
  indices_are_sorted = FALSE,
  output_types = NULL
)
```

## Arguments

- operand, start_indices:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

- gather_dimension_numbers:

  (`GatherDimensionNumbers`)  
  The gather dimension numbers.

- slice_sizes:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The sizes of the slices to gather.

- indices_are_sorted:

  (`logical(1)`)  
  Whether indices are sorted.

- output_types:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [`ValueType`](https://r-xla.github.io/stablehlo/dev/reference/ValueType.md)
  \| `NULL`)  
  Output types known ahead of time (e.g. from type inference at trace
  time). When provided, type inference and its input validation are
  skipped.

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
