test_that("FuncInputs repr", {
  inputs <- FuncInputs(list(
    FuncInput(
      id = ValueId("x"),
      type = ValueType(TensorType(
        elt_type = TensorElementType(type = FloatType("f32")),
        shape = Shape(c(1L, 2L))
      ))
    ),
    FuncInput(
      id = ValueId("y"),
      type = ValueType(TensorType(
        elt_type = TensorElementType(type = FloatType("f32")),
        shape = Shape(c(1L, 2L))
      ))
    )
  ))

  expect_equal(repr(inputs), "(%x: tensor<1x2xf32>, %y: tensor<1x2xf32>)")
})

test_that("Func repr", {
  inputs <- FuncInputs(list(
    FuncInput(
      id = ValueId("x"),
      type = ValueType(TensorType(
        elt_type = TensorElementType(type = FloatType("f32")),
        shape = Shape(c(1L, 2L))
      ))
    ),
    FuncInput(
      id = ValueId("y"),
      type = ValueType(TensorType(
        elt_type = TensorElementType(type = FloatType("f32")),
        shape = Shape(c(1L, 2L))
      ))
    )
  ))

  outputs <- FuncOutputs(list(
    FuncOutput(
      type = ValueType(TensorType(
        elt_type = TensorElementType(type = FloatType("f32")),
        shape = Shape(c(1L, 2L))
      ))
    )
  ))

  body <- FuncBody(list())

  func <- Func(
    id = FuncId("my_func"),
    inputs = inputs,
    outputs = outputs,
    body = body
  )

  expect_snapshot(repr(func))
})
