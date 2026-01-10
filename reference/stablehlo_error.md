# StablehloError

Base error class for all stablehlo errors

## Usage

``` r
stablehlo_error(
  call = sys.call(-1)[1L],
  ...,
  class = character(),
  signal = TRUE
)
```

## Arguments

- call:

  (`call` or `NULL`)  
  Call that generated the error

- ...:

  Additional fields to store in the condition

- class:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Additional classes to prepend

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
