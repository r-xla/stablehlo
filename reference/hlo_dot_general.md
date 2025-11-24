# DotGeneral Operator

See <https://openxla.org/stablehlo/spec#dot_general> for details.

## Usage

``` r
hlo_dot_general(lhs, rhs, contracting_dims, batching_dims = NULL)
```

## Arguments

- lhs, rhs, contracting_dims, batching_dims:

  ([`FuncVariable`](FuncVariable.md))  

## Value

[`FuncVariable`](FuncVariable.md)  
