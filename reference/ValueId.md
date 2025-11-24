# ValueId

This represents the name of a [`ValueType`](ValueType.md).

## Usage

``` r
ValueId(id = NULL)
```

## Arguments

- id:

  (`character(1)` or
  [`environment`](https://rdrr.io/r/base/environment.html))  
  Either a fixed name or an environment. If using an environment
  (default), the name will be generated automatically when calling
  [`repr()`](repr.md), i.e. the first value id will be `%0`, the second
  `%1`, etc..

## Value

(`ValueId`)
