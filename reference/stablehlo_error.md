# StablehloError

Base error class for all stablehlo errors

## Usage

``` r
stablehlo_error(message = character(), call = sys.call(-1), ..., signal = TRUE)
```

## Arguments

- message:

  (`character(1)`)  
  Error message

- call:

  (`call` or `NULL`)  
  Call that generated the error

- ...:

  Additional fields to store in the condition

- signal:

  (`logical(1)`)  
  Whether to signal the error (default TRUE)
