# Dimension Out of Range Error

Creates and optionally signals an error when a dimension index is
outside the valid range \[0, ndims).

## Usage

``` r
error_dimension_out_of_range(arg, dimension, ndims, call = NULL, signal = TRUE)
```

## Arguments

- arg:

  Name of the argument being checked.

- dimension:

  The dimension index(es) that are out of range (0-based).

- ndims:

  The number of dimensions of the tensor.

- call:

  The calling context for the error.

- signal:

  If TRUE (default), signals the error. If FALSE, returns the condition
  object.
