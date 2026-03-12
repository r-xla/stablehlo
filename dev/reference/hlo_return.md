# Return Values

Specifies the return values of a
[`Func`](https://r-xla.github.io/stablehlo/dev/reference/Func.md) and
finalize it.

## Usage

``` r
infer_types_return(...)

hlo_return(..., func = .current_func())
```

## Arguments

- ...:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  
  Return values. There must be at least one.

- func:

  ([`Func`](https://r-xla.github.io/stablehlo/dev/reference/Func.md))  
  The function. Default is to use
  [`.current_func()`](https://r-xla.github.io/stablehlo/dev/reference/dot-current_func.md).

## Value

([`Func`](https://r-xla.github.io/stablehlo/dev/reference/Func.md))
