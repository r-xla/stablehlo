# DimensionOutOfRangeError

Error when a dimension index is outside the valid range \[0, ndims)

## Usage

``` r
dimension_out_of_range_error(
  arg,
  dimension,
  ndims,
  call = sys.call(-1),
  signal = TRUE
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

- call:

  (`call` or `NULL`)  
  Call that generated the error

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
