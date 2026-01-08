# Invalid Identifier Error

Creates and optionally signals an error when an identifier doesn't
follow naming rules.

## Usage

``` r
error_invalid_identifier(arg, call = NULL, signal = TRUE)
```

## Arguments

- arg:

  The invalid identifier string.

- call:

  The calling context for the error.

- signal:

  If TRUE (default), signals the error. If FALSE, returns the condition
  object.
