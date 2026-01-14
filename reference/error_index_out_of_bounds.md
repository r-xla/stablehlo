# ErrorIndexOutOfBounds

Error when an index is outside the valid range \[lower, upper)

## Usage

``` r
error_index_out_of_bounds(
  arg,
  index,
  lower,
  upper,
  call = sys.call(-1),
  class = character(),
  signal = TRUE
)
```

## Arguments

- arg:

  (`character(1)`)  
  Name of the argument that caused the error

- index:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The observed index value(s).

- lower:

  (`integer(1)`)  
  Lower bound of valid range.

- upper:

  (`integer(1)`)  
  Upper bound of valid range, exclusive.

- call:

  (`call` or `NULL`)  
  Call that generated the error

- class:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Additional classes to prepend

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
