simple_type <- function(name, type = FloatType("f32"), shape = integer()) {
  ValueType(TensorType(TensorElementType(type), shape = Shape(shape)))
}

test_that("builder works", {
  id_x <- ValueId("x")
  type_x <- simple_type("x", shape = c(2L, 2L))

  x <- PartialFuncPointer(
    value_id = id_x,
    value_type = type_x,
    partial_func = PartialFunc$new(
      inputs = FuncInputs(list(FuncInput(id = id_x, type = type_x))),
      id = "main"
    )
  )

  id_y <- ValueId("y")
  type_y <- simple_type("y", shape = c(1L, 2L))
  y <- PartialFuncPointer(
    value_id = id_y,
    value_type = type_y,
    partial_func = PartialFunc$new(
      inputs = FuncInputs(list(FuncInput(id = id_y, type = type_y)))
    )
  )

  z <- stablehlo_add(x, y)
  out <- stablehlo_return(z)
  cat(repr(out))
})

test_that("return works", {
})


test_that("branching works", {
  id_cond <- ValueId("cond")
  id_branch1 <- ValueId("branch1")
  id_branch2 <- ValueId("branch2")

})