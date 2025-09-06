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

# specify shape in hlo_tensor

    Code
      repr(hlo_tensor(1:2, shape = c(2, 1))@func)
    Output
      [1] "func.func @main () ->  {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<[[1], [2]]> : tensor<2x1xi32>\n}: () -> (tensor<2x1xi32>)\n}\n"

---

    Code
      repr(hlo_tensor(1:2)@func)
    Output
      [1] "func.func @main () ->  {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<[1, 2]> : tensor<2xi32>\n}: () -> (tensor<2xi32>)\n}\n"

---

    Code
      repr(hlo_tensor(1)@func)
    Output
      [1] "func.func @main () ->  {\n%0 = \"stablehlo.constant\" () {\nvalue = dense<[1.00000000e+00]> : tensor<1xf32>\n}: () -> (tensor<1xf32>)\n}\n"

