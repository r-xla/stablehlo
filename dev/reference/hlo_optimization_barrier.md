# OptimizationBarrier Operator

See <https://openxla.org/stablehlo/spec#optimization_barrier> for
details.

## Usage

``` r
infer_types_optimization_barrier(...)

hlo_optimization_barrier(..., simplify = TRUE, output_types = NULL)
```

## Arguments

- ...:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

- simplify:

  (`logical(1)`)  
  Whether to simplify results by unpacking lists of length 1 into their
  single element.

- output_types:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [`ValueType`](https://r-xla.github.io/stablehlo/dev/reference/ValueType.md)
  \| `NULL`)  
  Output types known ahead of time (e.g. from type inference at trace
  time). When provided, type inference and its input validation are
  skipped.

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
