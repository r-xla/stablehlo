# Slice Index Error

Creates and optionally signals an error when slice indices
(start_indices, limit_indices) are invalid.

## Usage

``` r
error_slice_index(arg, indices, index_type, call = NULL, signal = TRUE)
```

## Arguments

- arg:

  Name of the argument being checked.

- indices:

  The invalid indices (0-based).

- index_type:

  Type of index: "start" or "limit".

- call:

  The calling context for the error.

- signal:

  If TRUE (default), signals the error. If FALSE, returns the condition
  object.
