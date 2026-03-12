# ErrorUnequalTypes

Error when types at the same index in two lists don't match

## Usage

``` r
error_unequal_types(
  arg1,
  arg2,
  index,
  expected,
  actual1,
  actual2,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
)
```

## Arguments

- arg1:

  (`character(1)`)  
  Name of the first argument

- arg2:

  (`character(1)`)  
  Name of the second argument

- index:

  (`integer(1)`)  
  The index where types don't match (0-based)

- expected:

  (`character(1)`)  
  Description of what was expected

- actual1:

  Type from the first argument (any object with a cli_format method)

- actual2:

  Type from the second argument (any object with a cli_format method)

- call:

  (`call` or `NULL`)  
  Call that generated the error

- class:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Additional classes to prepend

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
