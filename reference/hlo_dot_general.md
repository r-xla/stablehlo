# DotGeneral Operator

See <https://openxla.org/stablehlo/spec#dot_general> for details.

## Usage

``` r
infer_types_dot_general(lhs, rhs, dot_dimension_numbers)

hlo_dot_general(lhs, rhs, contracting_dims, batching_dims = NULL)
```

## Arguments

- lhs, rhs, contracting_dims, batching_dims:

  ([`FuncVariable`](FuncVariable.md))  

- dot_dimension_numbers:

  (`DotDimensionNumbers`)  
  The dot dimension number.

## Value

[`FuncVariable`](FuncVariable.md)  
