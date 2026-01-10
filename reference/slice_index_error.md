# SliceIndexError

Error when slice indices are invalid

## Usage

``` r
slice_index_error(
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
  The invalid index value(s) (0-based)

- lower:

  (`integer(1)`)  
  Lower bound of valid range (0-based)

- upper:

  (`integer(1)`)  
  Upper bound of valid range, exclusive (0-based)

- call:

  (`call` or `NULL`)  
  Call that generated the error

- class:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Additional classes to prepend

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
