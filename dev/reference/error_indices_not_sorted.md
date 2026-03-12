# ErrorIndicesNotSorted

Error when indices are not sorted in ascending order

## Usage

``` r
error_indices_not_sorted(
  arg,
  indices,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
)
```

## Arguments

- arg:

  (`character(1)`)  
  Name of the argument that caused the error

- indices:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The indices that are not sorted.

- call:

  (`call` or `NULL`)  
  Call that generated the error

- class:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Additional classes to prepend

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
