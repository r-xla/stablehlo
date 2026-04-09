# Module

A StableHLO module containing one or more
[`Func`](https://r-xla.github.io/stablehlo/dev/reference/Func.md)
objects. Modules allow defining multiple named functions, where
functions can call each other using
[`hlo_call`](https://r-xla.github.io/stablehlo/dev/reference/hlo_call.md).

Note: Module uses reference semantics like
[`Func`](https://r-xla.github.io/stablehlo/dev/reference/Func.md).

## Usage

``` r
Module(funcs = list())
```

## Arguments

- funcs:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [`Func`](https://r-xla.github.io/stablehlo/dev/reference/Func.md))  
  The functions in the module.

## Value

A `Module` object.
