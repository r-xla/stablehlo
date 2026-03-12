# Compare Operator

See <https://openxla.org/stablehlo/spec#compare> for details.

## Usage

``` r
infer_types_compare(lhs, rhs, comparison_direction, compare_type)

hlo_compare(lhs, rhs, comparison_direction, compare_type)
```

## Arguments

- lhs, rhs, comparison_direction, compare_type:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
