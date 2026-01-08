# Unequal Tensor Types Error

Creates and optionally signals an error when tensors have different
types but are expected to match.

## Usage

``` r
unequal_tensor_types_error(args, call = NULL, signal = TRUE)
```

## Arguments

- args:

  Named list of tensor types that should be equal.

- call:

  The calling context for the error.

- signal:

  If TRUE (default), signals the error. If FALSE, returns the condition
  object.
