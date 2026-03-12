# Cholesky Operator

See <https://openxla.org/stablehlo/spec#cholesky> for details.

## Usage

``` r
hlo_cholesky(operand, lower)
```

## Arguments

- operand, lower:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  

## Details

The values of the other half of the matrix are not guaranteed and
backend dependent.
