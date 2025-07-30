# op_constant

    Code
      repr(op_constant(3.14))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<+3.1400000000000001e+00> : tensor<f32> }\n:() -> (tensor<f32>)"

---

    Code
      repr(op_constant(3L))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<3> : tensor<i64> }\n:() -> (tensor<i64>)"

---

    Code
      repr(op_constant(-3L))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<-3> : tensor<i64> }\n:() -> (tensor<i64>)"

---

    Code
      repr(op_constant(-100L))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<-100> : tensor<i64> }\n:() -> (tensor<i64>)"

---

    Code
      repr(op_constant(TRUE))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<true> : tensor<i1> }\n:() -> (tensor<i1>)"

---

    Code
      repr(op_constant(FALSE))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<false> : tensor<i1> }\n:() -> (tensor<i1>)"

---

    Code
      repr(op_constant(3.14, elt_type = "f64"))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<+3.1400000000000001e+00> : tensor<f64> }\n:() -> (tensor<f64>)"

---

    Code
      repr(op_constant(3L, elt_type = "i32"))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<3> : tensor<i32> }\n:() -> (tensor<i32>)"

---

    Code
      repr(op_constant(TRUE, elt_type = "pred"))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<true> : tensor<i1> }\n:() -> (tensor<i1>)"

# Can create a function with no inputs

    Code
      repr(f)
    Output
      [1] "func.func @ () -> tensor<f32> {\n%1 =\"stablehlo.constant\"(){\nvalue = dense<+3.1400000000000001e+00> : tensor<f32> }\n:() -> (tensor<f32>)\n\"func.return\"(%1):(tensor<f32>) -> ()\n}\n"

# op_constant works with boolean values

    Code
      repr(op_true)
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<true> : tensor<i1> }\n:() -> (tensor<i1>)"

---

    Code
      repr(op_false)
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<false> : tensor<i1> }\n:() -> (tensor<i1>)"

