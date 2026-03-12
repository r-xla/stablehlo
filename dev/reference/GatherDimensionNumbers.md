# GatherDimensionNumbers

Represents the gather dimension numbers.

## Usage

``` r
GatherDimensionNumbers(
  offset_dims,
  collapsed_slice_dims,
  operand_batching_dims = integer(),
  start_indices_batching_dims = integer(),
  start_index_map,
  index_vector_dim
)
```

## Arguments

- offset_dims:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The offset dimensions.

- collapsed_slice_dims:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The collapsed slice dimensions.

- operand_batching_dims:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The operand batching dimensions.

- start_indices_batching_dims:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The start indices batching dimensions.

- start_index_map:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Maps start indices to operand dimensions.

- index_vector_dim:

  (`integer(1)`)  
  The index vector dimension.
