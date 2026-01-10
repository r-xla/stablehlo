# ErrorPermuteIndex

Error when permutation values are invalid (not a valid permutation of
indices)

## Usage

``` r
error_permute_index(
  arg,
  permutation,
  expected,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
)
```

## Arguments

- arg:

  (`character(1)`)  
  Name of the argument that caused the error

- permutation:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The permutation values that are invalid (0-based)

- expected:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The expected indices to be permuted (0-based)

- call:

  (`call` or `NULL`)  
  Call that generated the error

- class:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Additional classes to prepend

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
