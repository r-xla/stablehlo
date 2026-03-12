# Reduce Operator

See <https://openxla.org/stablehlo/spec#reduce> for details.

## Usage

``` r
infer_types_reduce(inputs, init_values, body, dimensions)

hlo_reduce(inputs, init_values, dimensions, body)
```

## Arguments

- inputs, init_values, dimensions, body:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)  
