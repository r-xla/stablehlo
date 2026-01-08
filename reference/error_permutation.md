# Permutation Error

Creates and optionally signals an error when permutation values are
invalid.

## Usage

``` r
error_permutation(arg, permutation, ndims, call = NULL, signal = TRUE)
```

## Arguments

- arg:

  Name of the argument being checked.

- permutation:

  The permutation values that are invalid (0-based).

- ndims:

  The number of dimensions of the tensor.

- call:

  The calling context for the error.

- signal:

  If TRUE (default), signals the error. If FALSE, returns the condition
  object.
