# Shape Mismatch Error

Creates and optionally signals an error when dimension sizes don't match
between two tensors.

## Usage

``` r
error_shape_mismatch(
  arg_lhs,
  arg_rhs,
  dim_lhs,
  dim_rhs,
  size_lhs,
  size_rhs,
  call = NULL,
  signal = TRUE
)
```

## Arguments

- arg_lhs:

  Name of the left-hand side argument.

- arg_rhs:

  Name of the right-hand side argument.

- dim_lhs:

  Dimension index in the left-hand side tensor (0-based).

- dim_rhs:

  Dimension index in the right-hand side tensor (0-based).

- size_lhs:

  Size of the dimension in the left-hand side tensor.

- size_rhs:

  Size of the dimension in the right-hand side tensor.

- call:

  The calling context for the error.

- signal:

  If TRUE (default), signals the error. If FALSE, returns the condition
  object.
