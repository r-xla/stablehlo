# Create a Closure

Creates a new function without any arguments that captures the provided
variables.

## Usage

``` r
hlo_closure(...)
```

## Arguments

- ...:

  ([`FuncValue`](FuncValue.md))  
  The variables to capture.

## Value

([`list()`](https://rdrr.io/r/base/list.html) of
[`FuncValue`](FuncValue.md))

## Examples

``` r
func <- local_func()
x <- hlo_input("x", "f32", shape = c(2, 2))
#> Error in .current_func(): No function is currently being built
y <- hlo_input("y", "f32", shape = c(2, 2))
#> Error in .current_func(): No function is currently being built
f <- hlo_closure(x, y)
#> Error: object 'x' not found
print(f)
#> Error: object 'f' not found
```
