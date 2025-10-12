# scalars

    Code
      repr(f@func@body@items[[1]])
    Output
      [1] "%0 = \"stablehlo.constant\" () {\nvalue = dense<3.14000000e+00> : tensor<f32>\n}: () -> (tensor<f32>)"

---

    Code
      repr(f@func@body@items[[1]])
    Output
      [1] "%0 = \"stablehlo.constant\" () {\nvalue = dense<3.1400000000000001e+00> : tensor<f64>\n}: () -> (tensor<f64>)"

---

    Code
      repr(f@func@body@items[[1]])
    Output
      [1] "%0 = \"stablehlo.constant\" () {\nvalue = dense<3> : tensor<i32>\n}: () -> (tensor<i32>)"

---

    Code
      repr(f@func@body@items[[1]])
    Output
      [1] "%0 = \"stablehlo.constant\" () {\nvalue = dense<3> : tensor<i64>\n}: () -> (tensor<i64>)"

---

    Code
      repr(f@func@body@items[[1]])
    Output
      [1] "%0 = \"stablehlo.constant\" () {\nvalue = dense<3> : tensor<i16>\n}: () -> (tensor<i16>)"

---

    Code
      repr(f@func@body@items[[1]])
    Output
      [1] "%0 = \"stablehlo.constant\" () {\nvalue = dense<3> : tensor<ui32>\n}: () -> (tensor<ui32>)"

---

    Code
      repr(f@func@body@items[[1]])
    Output
      [1] "%0 = \"stablehlo.constant\" () {\nvalue = dense<3> : tensor<ui64>\n}: () -> (tensor<ui64>)"

---

    Code
      repr(f@func@body@items[[1]])
    Output
      [1] "%0 = \"stablehlo.constant\" () {\nvalue = dense<3> : tensor<ui16>\n}: () -> (tensor<ui16>)"

---

    Code
      repr(f@func@body@items[[1]])
    Output
      [1] "%0 = \"stablehlo.constant\" () {\nvalue = dense<-3> : tensor<i32>\n}: () -> (tensor<i32>)"

---

    Code
      repr(f@func@body@items[[1]])
    Output
      [1] "%0 = \"stablehlo.constant\" () {\nvalue = dense<-100> : tensor<i32>\n}: () -> (tensor<i32>)"

---

    Code
      repr(f@func@body@items[[1]])
    Output
      [1] "%0 = \"stablehlo.constant\" () {\nvalue = dense<true> : tensor<i1>\n}: () -> (tensor<i1>)"

---

    Code
      repr(f@func@body@items[[1]])
    Output
      [1] "%0 = \"stablehlo.constant\" () {\nvalue = dense<false> : tensor<i1>\n}: () -> (tensor<i1>)"

# arrays

    Code
      repr(hlo_tensor(array(1:2), func = hlo_func())@func)
    Output
      [1] "func.func @main () ->  {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<[1, 2]> : tensor<2xi32>\n}: () -> (tensor<2xi32>)\n}\n"

---

    Code
      repr(hlo_tensor(array(1:6, dim = c(2, 3)), func = hlo_func())@func)
    Output
      [1] "func.func @main () ->  {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<[[1, 3, 5], [2, 4, 6]]> : tensor<2x3xi32>\n}: () -> (tensor<2x3xi32>)\n}\n"

---

    Code
      repr(hlo_tensor(array(1:6, dim = c(2, 3, 1)), func = hlo_func())@func)
    Output
      [1] "func.func @main () ->  {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<[[[1], [3], [5]], [[2], [4], [6]]]> : tensor<2x3x1xi32>\n}: () -> (tensor<2x3x1xi32>)\n}\n"

# specify shape in hlo_tensor

    Code
      repr(hlo_tensor(1:2, shape = c(2, 1), func = hlo_func())@func)
    Output
      [1] "func.func @main () ->  {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<[[1], [2]]> : tensor<2x1xi32>\n}: () -> (tensor<2x1xi32>)\n}\n"

---

    Code
      repr(hlo_tensor(1:2, func = hlo_func())@func)
    Output
      [1] "func.func @main () ->  {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<[1, 2]> : tensor<2xi32>\n}: () -> (tensor<2xi32>)\n}\n"

---

    Code
      repr(hlo_tensor(1, func = hlo_func())@func)
    Output
      [1] "func.func @main () ->  {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<[1.00000000e+00]> : tensor<1xf32>\n}: () -> (tensor<1xf32>)\n}\n"

# PJRTBuffer

    Code
      repr(hlo_tensor(pjrt_buffer(1L, dtype = "i32"), func = hlo_func())@func)
    Output
      [1] "func.func @main () ->  {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<[1]> : tensor<1xi32>\n}: () -> (tensor<1xi32>)\n}\n"

---

    Code
      repr(hlo_scalar(pjrt_scalar(1, dtype = "f32"), func = hlo_func())@func)
    Output
      [1] "func.func @main () ->  {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<1.00000000e+00> : tensor<f32>\n}: () -> (tensor<f32>)\n}\n"

# empty array: dense<[]> formatting

    Code
      repr(f)
    Output
      [1] "func.func @main () -> tensor<0xi64> {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<[]> : tensor<0xi64>\n}: () -> (tensor<0xi64>)\n\"func.return\"(%0): (tensor<0xi64>) -> ()\n}\n"

# empty array: array<> formatting

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<i64>) -> tensor<i64> {\n%0 = \"stablehlo.transpose\" (%x) {\npermutation = array<i64>\n}: (tensor<i64>) -> (tensor<i64>)\n\"func.return\"(%0): (tensor<i64>) -> ()\n}\n"

# scalar constant with hlo_tensor

    Code
      repr(hlo_tensor(1L, shape = integer())@func)
    Output
      [1] "func.func @main () ->  {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<1> : tensor<i32>\n}: () -> (tensor<i32>)\n}\n"

