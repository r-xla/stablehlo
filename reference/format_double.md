# Format Double Array with Scientific Notation

Formats a double array using scientific notation with enough digits to
round-trip at the requested precision. For 32 bit, the values are first
rounded to the nearest single-precision float, so the parser's own
rounding cannot round them a second time.

This is used to embed floating point constants into stableHLO programs.

## Usage

``` r
format_double(x, precision = 64)
```

## Arguments

- x:

  ([`double()`](https://rdrr.io/r/base/double.html))  
  Vector to format.

- precision:

  (`integer(1)`)  
  Currently supports 32 and 64 bit precisions.

## Value

[`character()`](https://rdrr.io/r/base/character.html)

## Examples

``` r
format_double(1.23, 32)
#> [1] "1.23000002e+00"
format_double(1.23, 64)
#> [1] "1.2300000000000000e+00"
```
