# SliceIndexError

Error when slice indices (start_indices, limit_indices) are invalid

## Usage

``` r
SliceIndexError(
  message = character(),
  call = NULL,
  .data = list(),
  arg = character(0),
  indices = integer(0),
  index_type = character(0)
)
```

## Arguments

- message:

  (`character(1)`)  
  Error message

- call:

  (`call` or `NULL`)  
  Call that generated the error

- .data:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Additional data to store in the condition

- arg:

  (`character(1)`)  
  Name of the argument that caused the error

- indices:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The invalid indices (0-based)

- index_type:

  (`character(1)`)  
  Type of index: "start" or "limit"
