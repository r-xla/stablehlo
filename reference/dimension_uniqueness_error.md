# DimensionUniquenessError

Error when dimension indices are not unique

## Usage

``` r
dimension_uniqueness_error(arg, dimensions, call = sys.call(-1), signal = TRUE)
```

## Arguments

- arg:

  (`character(1)`)  
  Name of the argument that caused the error

- dimensions:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The dimension indices that are not unique (0-based)

- call:

  (`call` or `NULL`)  
  Call that generated the error

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
