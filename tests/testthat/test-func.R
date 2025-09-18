test_that("FuncInputs repr", {
  inputs <- FuncInputs(list(
    FuncInput(
      id = ValueId("x"),
      type = ValueType(TensorType(
        dtype = FloatType("f32"),
        shape = Shape(c(1L, 2L))
      ))
    ),
    FuncInput(
      id = ValueId("y"),
      type = ValueType(TensorType(
        dtype = FloatType("f32"),
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
        dtype = FloatType("f32"),
        shape = Shape(c(1L, 2L))
      ))
    ),
    FuncInput(
      id = ValueId("y"),
      type = ValueType(TensorType(
        dtype = FloatType("f32"),
        shape = Shape(c(1L, 2L))
      ))
    )
  ))

  outputs <- FuncOutputs(list(
    FuncOutput(
      type = ValueType(TensorType(
        dtype = FloatType("f32"),
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

test_that("multiple returns", {
  func <- local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  f <- hlo_return(x, x)
  expect_snapshot(repr(f))
  skip_if_not_installed("pjrt")
  exec <- pjrt_compile(pjrt_program(repr(f)))
  out <- pjrt_execute(exec, pjrt_buffer(array(1:4, dim = c(2, 2))))
  expect_identical(out[[1L]], out[[2L]])
})

test_that("hlo_func", {
  globals[["CURRENT_FUNC"]] <- NULL
  f <- (function() {
    hlo_func("abc")
  })()
  expect_false(is.null(globals[["CURRENT_FUNC"]]))
  expect_true(inherits(f, Func))
  expect_equal(f@id@id, "abc")
})

test_that("local_func", {
  f1 <- local_func("f1")
  g <- function() {
    f2 <- local_func("f2")
    (function() local_func("f3"))()
    expect_equal(.current_func(), f2)
  }
  expect_equal(f1, .current_func())
})

test_that("hlo_func discards previous func", {
  f1 <- hlo_func("f1")
  f2 <- hlo_func("f2")
  hlo_return(hlo_input("x", "f32", shape = c(2, 2)))
  expect_error(.current_func(), "is currently being built")
})
