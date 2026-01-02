# InferenceError

Base class for type inference errors

## Usage

``` r
InferenceError(message = character(), call = NULL, .data = list())
```

## Arguments

- message:

  (`character(1)`)  
  Error message

- call:

  (`call` or `NULL`)  
  Call that generated the error

- .data:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Additional data to store in the condition
