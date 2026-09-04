# Shape

Represents the shape of a tensor: the size of each axis, `NA` where a
size is only known at run time.

A `Shape` *is* its integer vector, with a class attached, so
[`length()`](https://rdrr.io/r/base/length.html) is the rank and `[`
selects axes without unwrapping anything first.

## Usage

``` r
Shape(dims = integer())
```

## Arguments

- dims:

  ([`integer()`](https://rdrr.io/r/base/integer.html))

## Value

`Shape`
