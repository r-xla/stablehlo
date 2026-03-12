# ErrorUnexpectedListType

Error when an element in a list has an unexpected type

## Usage

``` r
error_unexpected_list_type(
  arg,
  index,
  expected,
  actual,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
)
```

## Arguments

- arg:

  (`character(1)`)  
  Name of the argument

- index:

  (`integer(1)`)  
  The index where the type is unexpected (0-based)

- expected:

  (`character(1)`)  
  Description of what was expected

- actual:

  (`character(1)`)  
  What was observed

- call:

  (`call` or `NULL`)  
  Call that generated the error

- class:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Additional classes to prepend

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
