# Create a input to a function

Create a input to a function

## Usage

``` r
hlo_input(name, dtype, shape = integer(), func = .current_func(), alias = NULL)
```

## Arguments

- name:

  (`character(1)`)  
  The name of the parameter.

- dtype:

  ([`ValueType`](ValueType.md))  
  The data type of the parameter. Can contain digits, letters and
  underscores. If it starts with a digit, it can only contain digits.
  Otherwise it must start with a letter.

- shape:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The shape of the parameter. Use
  [`integer()`](https://rdrr.io/r/base/integer.html) for scalars.

- func:

  ([`Func`](Func.md))  
  The function id of the parameter. Per default, uses the last function
  created with [`hlo_func`](hlo_func.md).

- alias:

  (`integer(1)` or `NULL`)  
  If integer, marks this input as alias with the given output index
  (0-based).

## Examples

``` r
func <- hlo_func()
x <- hlo_input("x", "f32", shape = c(2, 2))
print(x)
#> Variable %x in:
#> func.func @main (%x: tensor<2x2xf32>) ->  {
#> 
#> }

# You can combine multiple inputs as follows:
c(
  hlo_input("x", "f32", shape = c(2, 2)),
  hlo_input("y", "f32", shape = c(2, 2))
)
#> [[1]]
#> Variable %x in:
#> func.func @main (%x: tensor<2x2xf32>, %x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> 
#> }
#> 
#> [[2]]
#> Variable %y in:
#> func.func @main (%x: tensor<2x2xf32>, %x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> 
#> }
#> 
```
