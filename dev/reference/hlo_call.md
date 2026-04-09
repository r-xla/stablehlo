# Call a Function

Calls a named function from within the current function being built. The
callee must already be finalized via
[`hlo_return`](https://r-xla.github.io/stablehlo/dev/reference/hlo_return.md).

## Usage

``` r
infer_types_call(callee, ...)

hlo_call(callee, ..., simplify = TRUE)
```

## Arguments

- callee:

  ([`Func`](https://r-xla.github.io/stablehlo/dev/reference/Func.md))  
  The function to call. Must be a finalized
  [`Func`](https://r-xla.github.io/stablehlo/dev/reference/Func.md).

- ...:

  ([`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md))  
  The arguments to pass to the callee.

- simplify:

  (`logical(1)`)  
  If `TRUE` (default) and the callee has a single output, return a
  single
  [`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)
  instead of a list.

## Value

[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)
or [`list()`](https://rdrr.io/r/base/list.html) of
[`FuncValue`](https://r-xla.github.io/stablehlo/dev/reference/FuncValue.md)s.
