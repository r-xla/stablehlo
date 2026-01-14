# ErrorDimensionOutOfRange

Error when a dimension index is outside the valid range

## Usage

``` r
error_dimension_out_of_range(
  arg,
  dimension,
  dim_range,
  inclusive = c(TRUE, FALSE),
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
)
```

## Arguments

- arg:

  (`character(1)`)  
  Name of the argument that caused the error

- dimension:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  All dimension index(es). The error message will identify which ones
  are out of range.

- dim_range:

  (`integer(2)`)  
  The valid dimension range as c(min, max).

- inclusive:

  (`logical(2)`)  
  Whether lower and upper bounds are inclusive. Default is c(TRUE,
  FALSE) meaning \[lower, upper)

- call:

  (`call` or `NULL`)  
  Call that generated the error

- class:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Additional classes to prepend

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
