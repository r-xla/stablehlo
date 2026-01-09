# DimensionOutOfRangeError

Error when a dimension index is outside the valid range \[0, ndims)

## Usage

``` r
DimensionOutOfRangeError(
  arg,
  dimension,
  ndims,
  message = character(),
  call = NULL,
  ...
)
```

## Arguments

- arg:

  (`character(1)`)  
  Name of the argument that caused the error

- dimension:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The dimension index(es) that are out of range (0-based)

- ndims:

  (`integer(1)`)  
  The number of dimensions of the tensor

- message:

  (`character(1)`)  
  Error message

- call:

  (`call` or `NULL`)  
  Call that generated the error

- ...:

  Additional fields
