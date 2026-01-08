# Class Error

Creates and optionally signals an error when an object has an unexpected
class.

## Usage

``` r
error_class(arg, expected, observed, call = NULL, signal = TRUE)
```

## Arguments

- arg:

  Name of the argument being checked.

- expected:

  Character vector of expected class names.

- observed:

  The observed class name.

- call:

  The calling context for the error.

- signal:

  If TRUE (default), signals the error. If FALSE, returns the condition
  object.
