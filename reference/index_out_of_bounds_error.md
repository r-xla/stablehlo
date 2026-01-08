# Index Out of Bounds Error

Creates and optionally signals an error when an index is outside the
valid range \[lower, upper).

## Usage

``` r
index_out_of_bounds_error(arg, lower, upper, call = NULL, signal = TRUE)
```

## Arguments

- arg:

  Name of the argument being checked.

- lower:

  Lower bound of valid range (0-based).

- upper:

  Upper bound of valid range, exclusive (0-based).

- call:

  The calling context for the error.

- signal:

  If TRUE (default), signals the error. If FALSE, returns the condition
  object.
