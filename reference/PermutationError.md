# PermutationError

Error when permutation values are invalid (not a valid permutation of
dimension indices)

## Usage

``` r
PermutationError(
  arg,
  permutation,
  ndims,
  message = character(),
  call = NULL,
  ...
)
```

## Arguments

- arg:

  (`character(1)`)  
  Name of the argument that caused the error

- permutation:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The permutation values that are invalid (0-based)

- ndims:

  (`integer(1)`)  
  The number of dimensions of the tensor

- message:

  (`character(1)`)  
  Error message

- call:

  (`call` or `NULL`)  
  Call that generated the error

- ...:

  Additional fields
