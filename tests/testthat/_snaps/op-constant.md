# op_constant

    Code
      repr(op_constant(3.14))
    Output
      [1] "\"stablehlo.constant\"(){ value = dense<+3.1400000000000001e+00> : tensor<1xf32> }:() -> (tensor<1xf32>)"

---

    Code
      repr(op_constant(3L))
    Output
      [1] "\"stablehlo.constant\"(){ value = dense<3> : tensor<1xsi64> }:() -> (tensor<1xsi64>)"

---

    Code
      repr(op_constant(-3L))
    Output
      [1] "\"stablehlo.constant\"(){ value = dense<-3> : tensor<1xsi64> }:() -> (tensor<1xsi64>)"

---

    Code
      repr(op_constant(-100L))
    Output
      [1] "\"stablehlo.constant\"(){ value = dense<-100> : tensor<1xsi64> }:() -> (tensor<1xsi64>)"

---

    Code
      repr(op_constant(TRUE))
    Output
      [1] "\"stablehlo.constant\"(){ value = dense<true> : tensor<1xi1> }:() -> (tensor<1xi1>)"

---

    Code
      repr(op_constant(FALSE))
    Output
      [1] "\"stablehlo.constant\"(){ value = dense<false> : tensor<1xi1> }:() -> (tensor<1xi1>)"

---

    Code
      repr(op_constant(3.14, element_type = "f64"))
    Output
      [1] "\"stablehlo.constant\"(){ value = dense<+3.1400000000000001e+00> : tensor<1xf64> }:() -> (tensor<1xf64>)"

---

    Code
      repr(op_constant(3L, element_type = "s32"))
    Output
      [1] "\"stablehlo.constant\"(){ value = dense<3> : tensor<1xsi32> }:() -> (tensor<1xsi32>)"

---

    Code
      repr(op_constant(TRUE, element_type = "pred"))
    Output
      [1] "\"stablehlo.constant\"(){ value = dense<true> : tensor<1xi1> }:() -> (tensor<1xi1>)"

# Can create a function with no inputs

    Code
      repr(f)
    Output
      [1] "func.func @ () -> tensor<1xf32> {\n%1 =\"stablehlo.constant\"(){ value = dense<+3.1400000000000001e+00> : tensor<1xf32> }:() -> (tensor<1xf32>)\n\"func.return\"(%1):(tensor<1xf32>) -> ()\n}\n"

# op_constant works with boolean values

    Code
      repr(op_true)
    Output
      [1] "\"stablehlo.constant\"(){ value = dense<true> : tensor<1xi1> }:() -> (tensor<1xi1>)"

---

    Code
      repr(op_false)
    Output
      [1] "\"stablehlo.constant\"(){ value = dense<false> : tensor<1xi1> }:() -> (tensor<1xi1>)"

