# Tensor Number of Dimensions Error

Creates and optionally signals an error when a tensor has an unexpected
number of dimensions.

## Usage

``` r
error_tensor_ndims(arg, expected, observed, call = NULL, signal = TRUE)
```

## Arguments

- arg:

  Name of the argument being checked.

- expected:

  Integer vector of length 2 specifying \[lower, upper) bounds. Use NA
  for lower to mean no lower bound, NA for upper to mean no upper bound.
  Use c(n, n+1) to require exactly n dimensions.

- observed:

  The observed number of dimensions.

- call:

  The calling context for the error.

- signal:

  If TRUE (default), signals the error. If FALSE, returns the condition
  object.
