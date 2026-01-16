# Scatter Operator

See <https://openxla.org/stablehlo/spec#scatter> for details.

## Usage

``` r
infer_types_scatter(
  inputs,
  scatter_indices,
  updates,
  scatter_dimension_numbers,
  indices_are_sorted,
  unique_indices,
  update_computation
)

hlo_scatter(
  inputs,
  scatter_indices,
  updates,
  scatter_dimension_numbers,
  indices_are_sorted = FALSE,
  unique_indices = FALSE,
  update_computation
)
```

## Arguments

- inputs, scatter_indices, updates:

  ([`FuncValue`](FuncValue.md))  

- scatter_dimension_numbers:

  (`ScatterDimensionNumbers`)  
  The scatter dimension numbers.

- indices_are_sorted:

  (`logical(1)`)  
  Whether indices are sorted.

- unique_indices:

  (`logical(1)`)  
  Whether indices are unique.

- update_computation:

  ([`Func`](Func.md))  
  The update computation function.

## Value

[`FuncValue`](FuncValue.md)  
