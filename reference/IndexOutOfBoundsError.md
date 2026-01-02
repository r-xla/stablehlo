# IndexOutOfBoundsError

Error when an index is outside the valid range \[lower, upper)

## Usage

``` r
IndexOutOfBoundsError(
  message = character(),
  call = NULL,
  .data = list(),
  arg = character(0),
  lower = integer(0),
  upper = integer(0)
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

- lower:

  (`integer(1)`)  
  Lower bound of valid range (0-based)

- upper:

  (`integer(1)`)  
  Upper bound of valid range, exclusive (0-based)
