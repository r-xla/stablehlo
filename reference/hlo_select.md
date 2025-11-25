# Select Operator

See <https://openxla.org/stablehlo/spec#select> for details.

## Usage

``` r
infer_types_select(pred, on_true, on_false)

hlo_select(pred, on_true, on_false)
```

## Arguments

- pred, on_true, on_false:

  ([`FuncVariable`](FuncVariable.md))  

## Value

[`FuncVariable`](FuncVariable.md)  
