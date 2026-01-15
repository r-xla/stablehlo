# Reduce Operator

See <https://openxla.org/stablehlo/spec#reduce> for details.

## Usage

``` r
infer_types_reduce(inputs, init_values, body, dimensions)

hlo_reduce(inputs, init_values, dimensions, body)
```

## Arguments

- inputs, init_values, dimensions, body:

  ([`FuncValue`](FuncValue.md))  

## Value

[`FuncValue`](FuncValue.md)  
