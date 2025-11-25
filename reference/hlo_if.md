# If Operator

See <https://openxla.org/stablehlo/spec#if> for details.

## Usage

``` r
infer_types_if(pred, true_branch, false_branch)

hlo_if(pred, true_branch, false_branch)
```

## Arguments

- pred, true_branch, false_branch:

  ([`FuncVariable`](FuncVariable.md))  

## Value

[`FuncVariable`](FuncVariable.md)  
