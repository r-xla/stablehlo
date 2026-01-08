# Tensor Shape Error

Creates and optionally signals an error when a tensor has an unexpected
shape.

## Usage

``` r
error_tensor_shape(arg, expected, observed, call = NULL, signal = TRUE)
```

## Arguments

- arg:

  Name of the argument being checked.

- expected:

  Integer vector of expected shape dimensions.

- observed:

  Integer vector of observed shape dimensions.

- call:

  The calling context for the error.

- signal:

  If TRUE (default), signals the error. If FALSE, returns the condition
  object.
