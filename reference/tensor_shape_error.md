# Tensor Shape Error

Throws an error when a tensor has an unexpected shape.

## Usage

``` r
tensor_shape_error(arg, expected, observed, call = NULL)
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
