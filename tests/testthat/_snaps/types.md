# print methods - data types

    Code
      BooleanType()
    Output
      <i1>

---

    Code
      IntegerType(8)
    Output
      <i8>
    Code
      IntegerType(16)
    Output
      <i16>
    Code
      IntegerType(32)
    Output
      <i32>
    Code
      IntegerType(64)
    Output
      <i64>

---

    Code
      UnsignedType(8)
    Output
      <ui8>
    Code
      UnsignedType(16)
    Output
      <ui16>
    Code
      UnsignedType(32)
    Output
      <ui32>
    Code
      UnsignedType(64)
    Output
      <ui64>

---

    Code
      FloatType(32)
    Output
      <f32>
    Code
      FloatType(64)
    Output
      <f64>

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
      TensorType(BooleanType(), Shape(c()))
    Output
      tensor<i1> 
    Code
      TensorType(IntegerType(32), Shape(c(10)))
    Output
      tensor<10xi32> 
    Code
      TensorType(FloatType(64), Shape(c(2, 3, 4)))
    Output
      tensor<2x3x4xf64> 
    Code
      TensorType(UnsignedType(16), Shape(c(5, 6)))
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
      ValueTypes(list(ValueType(TensorType(IntegerType(32), Shape(c(2))))))
    Output
      <ValueTypes: tensor<2xi32>>

---

    Code
      ValueTypes(list(ValueType(TensorType(IntegerType(32), Shape(c(2)))), ValueType(
        TensorType(FloatType(32), Shape(c(3)))), ValueType(TensorType(BooleanType(),
      Shape(c())))))
    Output
      <ValueTypes[3]>
        [1] tensor<2xi32>
        [2] tensor<3xf32>
        [3] tensor<i1>

