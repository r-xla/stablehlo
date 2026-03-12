# Select Operator

See <https://openxla.org/stablehlo/spec#select> for details.

## Usage

``` r
infer_types_select(pred, on_true, on_false)

hlo_select(pred, on_true, on_false)
```

## Arguments

- pred, on_true, on_false:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
