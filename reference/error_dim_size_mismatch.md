# ErrorDimSizeMismatch

Error when a dimension size doesn't match the expected size at a given
index

## Usage

``` r
error_dim_size_mismatch(
  arg1,
  arg2,
  dim1,
  dim2,
  shape1,
  shape2,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
)
```

## Arguments

- arg1:

  (`character(1)`)  
  Name of the first argument (e.g. "operand")

- arg2:

  (`character(1)`)  
  Name of the second argument (e.g. "result")

- dim1:

  (`integer(1)`)  
  Dimension index in arg1 (0-based)

- dim2:

  (`integer(1)`)  
  Dimension index in arg2 (0-based)

- shape1:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Complete shape of arg1

- shape2:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Complete shape of arg2

- call:

  (`call` or `NULL`)  
  Call that generated the error

- class:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Additional classes to prepend

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
