# DimensionUniquenessError

Error when dimension indices are not unique

## Usage

``` r
DimensionUniquenessError(
  arg,
  dimensions,
  message = character(),
  call = NULL,
  ...
)
```

## Arguments

- arg:

  (`character(1)`)  
  Name of the argument that caused the error

- dimensions:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The dimension indices that are not unique (0-based)

- message:

  (`character(1)`)  
  Error message

- call:

  (`call` or `NULL`)  
  Call that generated the error

- ...:

  Additional fields
