# Dimension Uniqueness Error

Creates and optionally signals an error when dimension indices are not
unique.

## Usage

``` r
error_dimension_uniqueness(arg, dimensions, call = NULL, signal = TRUE)
```

## Arguments

- arg:

  Name of the argument being checked.

- dimensions:

  The dimension indices that are not unique (0-based).

- call:

  The calling context for the error.

- signal:

  If TRUE (default), signals the error. If FALSE, returns the condition
  object.
