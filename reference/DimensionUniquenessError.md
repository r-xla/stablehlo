# DimensionUniquenessError

Error when dimension indices are not unique

## Usage

``` r
DimensionUniquenessError(
  message = character(),
  call = NULL,
  .data = list(),
  arg = character(0),
  dimensions = integer(0)
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

- dimensions:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The dimension indices that are not unique (0-based)
