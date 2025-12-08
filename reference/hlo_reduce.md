# Reduce Operator

See <https://openxla.org/stablehlo/spec#reduce> for details.

## Usage

``` r
infer_types_reduce(..., body, dimensions)

hlo_reduce(inputs, init_values, dimensions, body)
```

## Arguments

- ...:

  (Inputs, Init values)

- inputs, init_values, dimensions, body:

  ([`FuncVariable`](FuncVariable.md))  

## Value

[`FuncVariable`](FuncVariable.md)  
