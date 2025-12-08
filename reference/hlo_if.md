# If Operator

See <https://openxla.org/stablehlo/spec#if> for details.

## Usage

``` r
infer_types_if(pred, true_branch, false_branch)

hlo_if(pred, true_branch, false_branch, simplify = TRUE)
```

## Arguments

- pred, true_branch, false_branch:

  ([`FuncVariable`](FuncVariable.md))  

- simplify:

  (`logical(1)`)  
  Whether to simplify results by unpacking lists of length 1 into their
  single element.

## Value

[`FuncVariable`](FuncVariable.md)  
