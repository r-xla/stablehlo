# scalars

    Code
      repr(op_constant(3.14))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<3.14000000e+00> : tensor<f32>\n}:() -> (tensor<f32>)"

---

    Code
      repr(op_constant(3.14, "f32"))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<3.14000000e+00> : tensor<f32>\n}:() -> (tensor<f32>)"

---

    Code
      repr(op_constant(3.14, "f64"))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<3.1400000000000001e+00> : tensor<f64>\n}:() -> (tensor<f64>)"

---

    Code
      repr(op_constant(3L))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<3> : tensor<i32>\n}:() -> (tensor<i32>)"

---

    Code
      repr(op_constant(3L, "i32"))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<3> : tensor<i32>\n}:() -> (tensor<i32>)"

---

    Code
      repr(op_constant(3L, "i64"))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<3> : tensor<i64>\n}:() -> (tensor<i64>)"

---

    Code
      repr(op_constant(3L, "i16"))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<3> : tensor<i16>\n}:() -> (tensor<i16>)"

---

    Code
      repr(op_constant(3L, "ui32"))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<3> : tensor<ui32>\n}:() -> (tensor<ui32>)"

---

    Code
      repr(op_constant(3L, "ui64"))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<3> : tensor<ui64>\n}:() -> (tensor<ui64>)"

---

    Code
      repr(op_constant(3L, "ui16"))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<3> : tensor<ui16>\n}:() -> (tensor<ui16>)"

---

    Code
      repr(op_constant(-3L))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<-3> : tensor<i32>\n}:() -> (tensor<i32>)"

---

    Code
      repr(op_constant(-100L))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<-100> : tensor<i32>\n}:() -> (tensor<i32>)"

---

    Code
      repr(op_constant(TRUE))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<true> : tensor<i1>\n}:() -> (tensor<i1>)"

---

    Code
      repr(op_constant(FALSE))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<false> : tensor<i1>\n}:() -> (tensor<i1>)"

# arrays

    Code
      repr(op_constant(array(1:2)))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<[1, 2]> : tensor<2xi32>\n}:() -> (tensor<2xi32>)"

---

    Code
      repr(op_constant(array(1:6, dim = c(2, 3))))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<[[1, 3, 5], [2, 4, 6]]> : tensor<2x3xi32>\n}:() -> (tensor<2x3xi32>)"

---

    Code
      repr(op_constant(array(1:6, dim = c(2, 3, 1))))
    Output
      [1] "\"stablehlo.constant\"(){\nvalue = dense<[[[1], [3], [5]], [[2], [4], [6]]]> : tensor<2x3x1xi32>\n}:() -> (tensor<2x3x1xi32>)"

