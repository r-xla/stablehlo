# InferenceError

Base class for type inference errors

## Usage

``` r
InferenceError(message = character(), call = NULL, ...)
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
