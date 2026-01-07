# Shape Mismatch Error

Throws an error when dimension sizes don't match between two tensors.

## Usage

``` r
shape_mismatch_error(
  arg_lhs,
  arg_rhs,
  dim_lhs,
  dim_rhs,
  size_lhs,
  size_rhs,
  call = NULL
)
```

## Arguments

- arg_lhs:

  Name of the left-hand side argument.

- arg_rhs:

  Name of the right-hand side argument.

- dim_lhs:

  Dimension index in the left-hand side tensor.

- dim_rhs:

  Dimension index in the right-hand side tensor.

- size_lhs:

  Size of the dimension in the left-hand side tensor.

- size_rhs:

  Size of the dimension in the right-hand side tensor.

- call:

  The calling context for the error.
