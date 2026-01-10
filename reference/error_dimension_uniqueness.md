# ErrorDimensionUniqueness

Error when dimension indices are not unique

## Usage

``` r
error_dimension_uniqueness(
  arg,
  dimensions,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
)
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

- class:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Additional classes to prepend

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
