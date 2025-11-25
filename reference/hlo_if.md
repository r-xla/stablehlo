# If Operator

See <https://openxla.org/stablehlo/spec#if> for details.

## Usage

``` r
infer_types_if(pred, true_branch, false_branch)

hlo_if(pred, true_branch, false_branch, simplify = TRUE)
```

## Arguments

- pred, true_branch, false_branch, simplify:

  ([`FuncVariable`](FuncVariable.md))  

- simplify:

  (`logical(1)`)  

## Value

[`FuncVariable`](FuncVariable.md)  
