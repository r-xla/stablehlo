# DimensionOutOfRangeError

Error when a dimension index is outside the valid range \[0, ndims)

## Usage

``` r
DimensionOutOfRangeError(
  message = character(),
  call = NULL,
  .data = list(),
  arg = character(0),
  dimension = integer(0),
  ndims = integer(0)
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

- dimension:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The dimension index(es) that are out of range (0-based)

- ndims:

  (`integer(1)`)  
  The number of dimensions of the tensor
