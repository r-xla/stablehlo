# Cholesky Operator

See <https://openxla.org/stablehlo/spec#cholesky> for details.

## Usage

``` r
hlo_cholesky(operand, lower)
```

## Arguments

- operand, lower:

  ([`FuncValue`](FuncValue.md))  

## Value

[`FuncValue`](FuncValue.md)  

## Details

The values of the other half of the matrix are not guaranteed and
backend dependent.
