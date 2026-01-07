# Get Started

In this vignette, we will give an overview of the {stablehlo} package
and show how it can be used to create StableHLO programs. If you are not
familiar with StableHLO, we recommend reading the [StableHLO
specification](https://openxla.org/stablehlo/spec) first.

The {stablehlo} package represents StableHLO programs as nested S3
objects. When working with the package, you usually don’t construct
these objects via their `S3`-constructor, but through the more
user-friendly `hlo_<name>` functions.

We start by creating a `Func` object that represents a StableHLO
function. Such a `Func` object has an ID (`FuncId`), inputs
(`FuncInputs`), outputs (`FuncOutputs`) and a body (`FuncBody`). Below,
we are calling our function `"main"` and create an empty function via
[`local_func()`](../reference/hlo_func.md).

``` r
library(stablehlo)
main_func <- local_func()
main_func
#> func.func @main () ->  {
#> 
#> }
```

This `Func` object is now accessible via
[`.current_func()`](../reference/dot-current_func.md).

``` r
.current_func()
#> func.func @main () ->  {
#> 
#> }
```

When we exit the scope where `func` was created and we haven’t finished
building the function (via [`hlo_return()`](../reference/hlo_return.md),
see later), this global state will be cleaned up. If you don’t want
this, use [`hlo_func()`](../reference/hlo_func.md), but unless you have
a good reason to do so, you should use
[`local_func()`](../reference/hlo_func.md).

To convert this (unfinished) function into its stableHLO string
representation, we can use the `repr` method, which is also called when
printing the `Func`.

``` r
repr(main_func)
#> [1] "func.func @main () ->  {\n\n}\n"
```

We can now add inputs to the function, which will populate the
`FuncInputs` of the `Func`. We create two inputs, `x` and `y`, and add
them to the function via [`hlo_input()`](../reference/hlo_input.md). We
start by adding an argument `x`, which is a `2x2xf32` tensor.

``` r
x <- hlo_input("x", "f32", shape = c(2, 2), func = main_func)
```

We don’t have to specify the `func` argument explicitly, because the
default is to use [`.current_func()`](../reference/dot-current_func.md).

``` r
y <- hlo_input("y", "f32", shape = c(2, 2))
```

The R variables `x` and `y` are now wrapped in a `FuncValue` object.
Such a `FuncValue` represents a value in a stableHLO function and
contains:

1.  The `ValueId` of the value.
2.  The `ValueType` of the value.
3.  The `Func` object that the value belongs to.

``` r
x
#> Variable %x in:
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> 
#> }
x$value_id
#> $id
#> [1] "x"
#> 
#> attr(,"class")
#> [1] "ValueId"
x$value_type
#> <ValueType: tensor<2x2xf32>>
x$func
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> 
#> }
```

Next, we will populate the body of the function with operations. We will
start by adding the values of `x` and `y` together. Because `Func` is a
reference object, this also updates the `main_func` from above.

``` r
z <- hlo_add(x, y)
z
#> Variable %0 in:
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> %0 = "stablehlo.add" (%x, %y): (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
#> }
```

The identifier of `z` (its `ValueId`) is automatically generated and you
don’t have to specify it explicitly. Next, we concatenate `z` and `x`
together along the second dimension (note that the dimension is 0-based
in stableHLO).

``` r
w <- hlo_concatenate(z, x, dimension = 1L)
w
#> Variable %1 in:
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> %0 = "stablehlo.add" (%x, %y): (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
#> %1 = "stablehlo.concatenate" (%0, %x) {
#> dimension = 1 : i64
#> }: (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x4xf32>)
#> }
```

We see above that the inputs to operations can not only be tensors, but
also other, “static” inputs. Inputs are static when they can’t be
changed during execution. Static inputs are either tensors (scalars are
0-dimensional tensors), or functions. To demonstrate using a function,
we create another (unnamed) function via
[`local_func()`](../reference/hlo_func.md).

``` r
reduce_func <- local_func()
z1 <- hlo_input("z1", "f32", shape = integer())
z2 <- hlo_input("z2", "f32", shape = integer())
```

Because our [`.current_func()`](../reference/dot-current_func.md) is now
`reduce_func`, the inputs are added to `reduce_func` and not `func`. If
we wanted to add them to `main_func`, we would have to explicitly
specify `func = main_func`.

``` r
.current_func()
#> func.func @main (%z1: tensor<f32>, %z2: tensor<f32>) ->  {
#> 
#> }
reduce_func
#> func.func @main (%z1: tensor<f32>, %z2: tensor<f32>) ->  {
#> 
#> }
```

Our reduction operation will just add `z1` and `z2` together.

``` r
out_reduce <- hlo_add(z1, z2)
out_reduce
#> Variable %0 in:
#> func.func @main (%z1: tensor<f32>, %z2: tensor<f32>) ->  {
#> %0 = "stablehlo.add" (%z1, %z2): (tensor<f32>, tensor<f32>) -> (tensor<f32>)
#> }
```

Now we are done specifying the body of the reduction function, so we can
return the result via [`hlo_return()`](../reference/hlo_return.md). You
should only call this once you are done building the function.

``` r
hlo_return(out_reduce)
#> func.func @main (%z1: tensor<f32>, %z2: tensor<f32>) -> tensor<f32> {
#> %0 = "stablehlo.add" (%z1, %z2): (tensor<f32>, tensor<f32>) -> (tensor<f32>)
#> "func.return"(%0): (tensor<f32>) -> ()
#> }
```

The output of `hlo_return` is the same (identical) object as the
`reduce_func` (`Func`s are reference objects).

In order to specify the reduce operation in our main body, we now only
need to define an initial scalar value of the same type as the tensor we
are reducing. We can do this via
[`hlo_scalar()`](../reference/hlo_constant.md).

``` r
init <- hlo_scalar(0, dtype = "f32")
init
#> Variable %2 in:
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> %0 = "stablehlo.add" (%x, %y): (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
#> %1 = "stablehlo.concatenate" (%0, %x) {
#> dimension = 1 : i64
#> }: (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x4xf32>)
#> %2 = "stablehlo.constant" () {
#> value = dense<0.00000000e+00> : tensor<f32>
#> }: () -> (tensor<f32>)
#> }
```

Next, we add the reduce operation to our main function and specify the
dimensions to reduce along.

``` r
out_main <- hlo_reduce(inputs = x, init_values = init, dimensions = c(0, 1L), body = reduce_func)
out_main
#> Variable %3 in:
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
#> %0 = "stablehlo.add" (%x, %y): (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
#> %1 = "stablehlo.concatenate" (%0, %x) {
#> dimension = 1 : i64
#> }: (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x4xf32>)
#> %2 = "stablehlo.constant" () {
#> value = dense<0.00000000e+00> : tensor<f32>
#> }: () -> (tensor<f32>)
#> %3 = "stablehlo.reduce" (%x, %2)({
#>   ^bb0(%z1: tensor<f32>, %z2: tensor<f32>):
#>     %4 = "stablehlo.add" (%z1, %z2): (tensor<f32>, tensor<f32>) -> (tensor<f32>)
#>     "stablehlo.return"(%4): (tensor<f32>) -> ()
#> }) {
#> dimensions = array<i64: 0, 1>
#> }: (tensor<2x2xf32>, tensor<f32>) -> (tensor<f32>)
#> }
```

Finally, we return the result of the main function via
[`hlo_return()`](../reference/hlo_return.md).

``` r
hlo_return(out_main)
#> func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) -> tensor<f32> {
#> %0 = "stablehlo.add" (%x, %y): (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
#> %1 = "stablehlo.concatenate" (%0, %x) {
#> dimension = 1 : i64
#> }: (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x4xf32>)
#> %2 = "stablehlo.constant" () {
#> value = dense<0.00000000e+00> : tensor<f32>
#> }: () -> (tensor<f32>)
#> %3 = "stablehlo.reduce" (%x, %2)({
#>   ^bb0(%z1: tensor<f32>, %z2: tensor<f32>):
#>     %4 = "stablehlo.add" (%z1, %z2): (tensor<f32>, tensor<f32>) -> (tensor<f32>)
#>     "stablehlo.return"(%4): (tensor<f32>) -> ()
#> }) {
#> dimensions = array<i64: 0, 1>
#> }: (tensor<2x2xf32>, tensor<f32>) -> (tensor<f32>)
#> "func.return"(%3): (tensor<f32>) -> ()
#> }
```

If we want to run this function, we need the {pjrt} package. You can
learn about the package in the [pjrt
documentation](https://r-xla.github.io/pjrt/articles/pjrt.html).

``` r
library(pjrt)
src <- repr(main_func)
program <- pjrt_program(src)
executable <- pjrt_compile(program)
```

Next, we create some input values and run the function.

``` r
x_buf <- pjrt_buffer(1:4, shape = c(2, 2), dtype = "f32")
x_buf
#> PJRTBuffer 
#>  1.0000 3.0000
#>  2.0000 4.0000
#> [ CPUf32{2x2} ]
y_buf <- pjrt_buffer(5:8, shape = c(2, 2), dtype = "f32")
y_buf
#> PJRTBuffer 
#>  5.0000 7.0000
#>  6.0000 8.0000
#> [ CPUf32{2x2} ]
out_buf <- pjrt_execute(executable, x_buf, y_buf)
out_buf
#> PJRTBuffer 
#>  10.0000
#> [ CPUf32{} ]
```
