# Select Operator

See <https://openxla.org/stablehlo/spec#select> for details.

## Usage

``` r
infer_types_select(pred, on_true, on_false)

hlo_select(pred, on_true, on_false, output_types = NULL)
```

## Arguments

- pred, on_true, on_false:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

- output_types:

  ([`list()`](https://rdrr.io/r/base/list.html) of
  [`ValueType`](https://r-xla.github.io/stablehlo/dev/reference/ValueType.md)
  \| `NULL`)  
  Output types known ahead of time (e.g. from type inference at trace
  time). When provided, type inference and its input validation are
  skipped.

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
