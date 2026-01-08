# Tensor Data Type Error

Creates and optionally signals an error when a tensor has an unexpected
data type.

## Usage

``` r
tensor_dtype_error(arg, expected, observed, call = NULL, signal = TRUE)
```

## Arguments

- arg:

  Name of the argument being checked.

- expected:

  Character vector of expected dtype names.

- observed:

  The observed dtype name.

- call:

  The calling context for the error.

- signal:

  If TRUE (default), signals the error. If FALSE, returns the condition
  object.
