# ErrorConcatenateShapes

Error when input shapes don't match (except at given dimensions)

## Usage

``` r
error_concatenate_shapes(
  dimensions,
  shapes,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
)
```

## Arguments

- dimensions:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The dimensions where shapes may differ (0-based)

- shapes:

  ([`list()`](https://rdrr.io/r/base/list.html) of `Shape`)  
  The input shapes

- call:

  (`call` or `NULL`)  
  Call that generated the error

- class:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Additional classes to prepend

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
