# print methods - compound types

    Code
      Shape(c())
    Output
      ()
    Code
      Shape(c(5))
    Output
      (5)
    Code
      Shape(c(2, 3, 4))
    Output
      (2x3x4)
    Code
      Shape(c(10, NA, 20))
    Output
      (10x?x20)

---

    Code
      TensorType(as_dtype("bool"), Shape(c()))
    Output
      tensor<i1> 
    Code
      TensorType(as_dtype("i32"), Shape(c(10)))
    Output
      tensor<10xi32> 
    Code
      TensorType(as_dtype("f64"), Shape(c(2, 3, 4)))
    Output
      tensor<2x3x4xf64> 
    Code
      TensorType(as_dtype("ui16"), Shape(c(5, 6)))
    Output
      tensor<5x6xui16> 

---

    Code
      TokenType()
    Output
      <TokenType: !stablehlo.token>

---

    Code
      ValueTypes(list())
    Output
      <ValueTypes: (empty)>

---

    Code
      ValueTypes(list(ValueType(TensorType(as_dtype("i32"), Shape(c(2))))))
    Output
      <ValueTypes: tensor<2xi32>>

---

    Code
      ValueTypes(list(ValueType(TensorType(as_dtype("i32"), Shape(c(2)))), ValueType(
        TensorType(as_dtype("f32"), Shape(c(3)))), ValueType(TensorType(as_dtype(
        "bool"), Shape(c())))))
    Output
      <ValueTypes[3]>
        [1] tensor<2xi32>
        [2] tensor<3xf32>
        [3] tensor<i1>

