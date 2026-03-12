# Get the last function created

Get the last function created (either via
[`hlo_func`](https://r-xla.github.io/stablehlo/dev/reference/hlo_func.md)
or
[`local_func`](https://r-xla.github.io/stablehlo/dev/reference/hlo_func.md)),
which is not returned yet.

## Usage

``` r
.current_func()
```

## Value

A [`Func`](https://r-xla.github.io/stablehlo/dev/reference/Func.md)
object.
