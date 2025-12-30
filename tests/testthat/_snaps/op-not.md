# basic tests

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<3x3xi1>) -> tensor<3x3xi1> {\n%0 = \"stablehlo.not\" (%x): (tensor<3x3xi1>) -> (tensor<3x3xi1>)\n\"func.return\"(%0): (tensor<3x3xi1>) -> ()\n}\n"

# error

    Code
      hlo_not(x)
    Condition
      Error in `hlo_not()`:
      ! Expected `operand` to have dtype BooleanType, IntegerType, or UnsignedType.
      i Got <FloatType(32)>.

