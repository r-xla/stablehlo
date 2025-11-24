# Create a function

Both functions create a new [`Func`](Func.md) with the given id which is
afterwards accessible via [`.current_func()`](dot-current_func.md).
Functions receiving a [`Func`](Func.md) as an argument (such as
[`hlo_input`](hlo_input.md), [`hlo_add`](hlo_add.md), ...) usually use
[`.current_func()`](dot-current_func.md) by default. You can also
directly create a function using [`Func()`](Func.md), which will *not*
be accessible this way.

Differences between the two functions:

- `local_func` removes the function when exiting the current scope,
  whereas `hlo_func` does not.

- `hlo_func` discards the previously built function(s), whereas
  `local_func` does not: after a function created by `local_func` is
  either cleaned up automatically (by exiting the scope) or the function
  is finalized via [`hlo_return`](hlo_return.md), the previously built
  function is restored, i.e., accessible via
  [`.current_func()`](dot-current_func.md). To build nested functions
  (e.g. to create a closure that is passed to another op), use
  `local_func` instead of `hlo_func`.

## Usage

``` r
hlo_func(id = "main")

local_func(id = "main", envir = parent.frame())
```

## Arguments

- id:

  (`character(1)`  
  The id of the function.

- envir:

  (`environment`)  
  Environment where exit handler will be registered for cleaning up the
  [`Func`](Func.md) if it was not returned yet.

## Value

A [`Func`](Func.md) object.
