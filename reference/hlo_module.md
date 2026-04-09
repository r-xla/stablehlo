# Create a module

Both functions create a new
[`Module`](https://r-xla.github.io/stablehlo/reference/Module.md) which
is afterwards accessible via
[`.current_module()`](https://r-xla.github.io/stablehlo/reference/dot-current_module.md).
Functions created with
[`hlo_func`](https://r-xla.github.io/stablehlo/reference/hlo_func.md) or
[`local_func`](https://r-xla.github.io/stablehlo/reference/hlo_func.md)
will automatically register into the current module. The module is
finalized when a function named `"main"` is returned via
[`hlo_return`](https://r-xla.github.io/stablehlo/reference/hlo_return.md).

Differences between the two functions:

- `local_module` removes the module when exiting the current scope,
  whereas `hlo_module` does not.

- `hlo_module` discards the previously built module(s), whereas
  `local_module` does not.

## Usage

``` r
hlo_module()

local_module(envir = parent.frame())
```

## Arguments

- envir:

  (`environment`)  
  Environment where exit handler will be registered for cleaning up the
  [`Module`](https://r-xla.github.io/stablehlo/reference/Module.md) if
  it was not finalized yet.

## Value

A [`Module`](https://r-xla.github.io/stablehlo/reference/Module.md)
object.
