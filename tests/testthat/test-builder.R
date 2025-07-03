test_that("builder works", {
  new_arg <- function(id, shape) {
    PartialFunc$new(
      id = NULL,
      inputs = FuncInputs(
        list(
          FuncInput(id = ValueId(id), type = ValueType(TensorType(TensorElementType(FloatType("f32")), shape = Shape(c(shape))))),
        )
      ),
      outputs = NULL,
      body = FuncBody(list())
    )
  }
  id_x <- ValueId("x")
  type_x <- ValueType(TensorType(TensorElementType(FloatType("f32")), shape = Shape(c(2L, 2L))))
  id_y <- ValueId("y")
  type_y <- ValueType(TensorType(TensorElementType(FloatType("f32")), shape = Shape(c(2L, 2L))))

  x <- PartialFuncPointer
})