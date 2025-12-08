# While Operator

See <https://openxla.org/stablehlo/spec#while> for details.

## Usage

``` r
infer_types_while(..., cond, body)

hlo_while(..., cond, body, simplify = TRUE)
```

## Arguments

- ..., cond, body:

  ([`FuncValue`](FuncValue.md))  

- simplify:

  (`logical(1)`)  
  Whether to simplify results by unpacking lists of length 1 into their
  single element.

## Value

[`FuncValue`](FuncValue.md)  
