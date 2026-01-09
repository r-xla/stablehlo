# IndexOutOfBoundsError

Error when an index is outside the valid range \[lower, upper)

## Usage

``` r
IndexOutOfBoundsError(
  arg,
  lower,
  upper,
  message = character(),
  call = NULL,
  ...
)
```

## Arguments

- arg:

  (`character(1)`)  
  Name of the argument that caused the error

- lower:

  (`integer(1)`)  
  Lower bound of valid range (0-based)

- upper:

  (`integer(1)`)  
  Upper bound of valid range, exclusive (0-based)

- message:

  (`character(1)`)  
  Error message

- call:

  (`call` or `NULL`)  
  Call that generated the error

- ...:

  Additional fields
