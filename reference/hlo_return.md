# Return Values

Specifies the return values of a [`Func`](Func.md) and finalize it.

## Usage

``` r
hlo_return(..., func = .current_func())

infer_types_return(...)
```

## Arguments

- ...:

  ([`FuncValue`](FuncValue.md))  
  Return values. There must be at least one.

- func:

  ([`Func`](Func.md))  
  The function. Default is to use
  [`.current_func()`](dot-current_func.md).

## Value

([`Func`](Func.md))
