# ScatterDimensionNumbers

Represents the scatter dimension numbers.

## Usage

``` r
ScatterDimensionNumbers(
  update_window_dims,
  inserted_window_dims,
  input_batching_dims = integer(),
  scatter_indices_batching_dims = integer(),
  scatter_dims_to_operand_dims,
  index_vector_dim
)
```

## Arguments

- update_window_dims:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The update window dimensions.

- inserted_window_dims:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The inserted window dimensions.

- input_batching_dims:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The input batching dimensions.

- scatter_indices_batching_dims:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The scatter indices batching dimensions.

- scatter_dims_to_operand_dims:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Maps scatter dimensions to operand dimensions.

- index_vector_dim:

  (`integer(1)`)  
  The index vector dimension.
