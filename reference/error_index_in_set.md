# ErrorIndexInSet

Error when an index is found in a forbidden set

## Usage

``` r
error_index_in_set(
  arg1,
  arg2,
  index,
  set,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
)
```

## Arguments

- arg1:

  (`character(1)`)  
  Name of the argument containing the index

- arg2:

  (`character(1)`)  
  Name of the argument containing the set

- index:

  (`integer(1)`)  
  The index that was found in the set

- set:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The set that should not contain the index

- call:

  (`call` or `NULL`)  
  Call that generated the error

- class:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Additional classes to prepend

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
