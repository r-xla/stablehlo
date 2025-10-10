# Func repr

    Code
      repr(func)
    Output
      [1] "func.func @my_func (%x: tensor<1x2xf32>, %y: tensor<1x2xf32>) -> tensor<1x2xf32> {\n\n}\n"

# multiple returns

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>) -> (tensor<2x2xf32>, tensor<2x2xf32>) {\n\"func.return\"(%x, %x): (tensor<2x2xf32>, tensor<2x2xf32>) -> ()\n}\n"

