# op_constant works with numeric values

    Code
      repr(op)
    Output
      [1] "\"stablehlo.constant\"(){ value = dense<+3.1400000000000001E+00> : tensor<1xf32> }:() -> (tensor<1xf32>)"

# Can create a function with no inputs

    Code
      repr(f)
    Output
      [1] "func.func @ () -> tensor<1xf32> {\n%1 =\"stablehlo.constant\"(){ value = dense<+3.1400000000000001E+00> : tensor<1xf32> }:() -> (tensor<1xf32>)\n\"func.return\"(%1):(tensor<1xf32>) -> ()\n}\n"

