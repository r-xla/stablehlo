# PermutationError

Error when permutation values are invalid (not a valid permutation of
dimension indices)

## Usage

``` r
PermutationError(
  message = character(),
  call = NULL,
  .data = list(),
  arg = character(0),
  permutation = integer(0),
  ndims = integer(0)
)
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

- arg:

  (`character(1)`)  
  Name of the argument that caused the error

- permutation:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The permutation values that are invalid (0-based)

- ndims:

  (`integer(1)`)  
  The number of dimensions of the tensor
