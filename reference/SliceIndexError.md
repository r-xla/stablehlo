# SliceIndexError

Error when slice indices (start_indices, limit_indices) are invalid

## Usage

``` r
SliceIndexError(
  arg,
  indices,
  index_type,
  message = character(),
  call = NULL,
  ...
)
```

## Arguments

- arg:

  (`character(1)`)  
  Name of the argument that caused the error

- indices:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The invalid indices (0-based)

- index_type:

  (`character(1)`)  
  Type of index: "start" or "limit"

- message:

  (`character(1)`)  
  Error message

- call:

  (`call` or `NULL`)  
  Call that generated the error

- ...:

  Additional fields
