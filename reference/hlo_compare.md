# Compare Operator

See <https://openxla.org/stablehlo/spec#compare> for details.

## Usage

``` r
infer_types_compare(lhs, rhs, comparison_direction, compare_type)

hlo_compare(lhs, rhs, comparison_direction, compare_type)
```

## Arguments

- lhs, rhs, comparison_direction, compare_type:

  ([`FuncVariable`](FuncVariable.md))  

## Value

[`FuncVariable`](FuncVariable.md)  
