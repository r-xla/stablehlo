test_that("infer_types_generic_biv errors", {
  check <- function(lhs, rhs) {
    expect_snapshot(infer_types_generic_biv(lhs, rhs), error = TRUE)
  }
  # lhs is not a ValueType
  check("not a value type", vt("i32", c(2L, 3L)))
  # lhs is not a TensorType
  check(ValueType(TokenType()), vt("i32", c(2L, 3L)))
  # lhs and rhs have different types
  check(vt("i32", c(2L, 3L)), vt("f32", c(2L, 3L)))
})

test_that("infer_types_numeric_biv errors", {
  check <- function(lhs, rhs) {
    expect_snapshot(infer_types_numeric_biv(lhs, rhs), error = TRUE)
  }
  # lhs has wrong dtype (boolean not allowed)
  check(vt("pred", c(2L, 3L)), vt("pred", c(2L, 3L)))
  # lhs and rhs have different types
  check(vt("i32", c(2L, 3L)), vt("i32", c(3L, 4L)))
})

test_that("infer_types_float_biv errors", {
  check <- function(lhs, rhs) {
    expect_snapshot(infer_types_float_biv(lhs, rhs), error = TRUE)
  }
  # lhs has wrong dtype
  check(vt("i32", c(2L, 3L)), vt("i32", c(2L, 3L)))
  # lhs and rhs have different types
  check(vt("f32", c(2L, 3L)), vt("f32", c(3L, 4L)))
})

test_that("infer_types_integerish_biv errors", {
  check <- function(lhs, rhs) {
    expect_snapshot(infer_types_integerish_biv(lhs, rhs), error = TRUE)
  }
  # lhs has wrong dtype (float not allowed)
  check(vt("f32", c(2L, 3L)), vt("f32", c(2L, 3L)))
  # rhs has wrong dtype
  check(vt("i32", c(2L, 3L)), vt("f32", c(2L, 3L)))
  # lhs and rhs have different types
  check(vt("i32", c(2L, 3L)), vt("i32", c(3L, 4L)))
})

test_that("infer_types_integer_biv errors", {
  check <- function(lhs, rhs) {
    expect_snapshot(infer_types_integer_biv(lhs, rhs), error = TRUE)
  }
  # floats are not integers
  check(vt("f32", c(2L, 3L)), vt("f32", c(2L, 3L)))
  # booleans are not integers either -- this is what separates it from
  # `infer_types_integerish_biv()`
  check(vt("i1", c(2L, 3L)), vt("i1", c(2L, 3L)))
  check(vt("i32", c(2L, 3L)), vt("i1", c(2L, 3L)))
  # lhs and rhs have different types
  check(vt("i32", c(2L, 3L)), vt("i32", c(3L, 4L)))
})

test_that("infer_types_integer_biv accepts signed and unsigned integers", {
  expect_no_error(infer_types_integer_biv(vt("i32", 2L), vt("i32", 2L)))
  expect_no_error(infer_types_integer_biv(vt("ui8", 2L), vt("ui8", 2L)))
})

test_that("infer_types_generic_uni errors", {
  check <- function(operand) {
    expect_snapshot(infer_types_generic_uni(operand), error = TRUE)
  }
  # operand is not a ValueType
  check("not a value type")
  # operand is not a TensorType
  check(ValueType(TokenType()))
})

test_that("infer_types_numeric_uni errors", {
  check <- function(operand) {
    expect_snapshot(infer_types_numeric_uni(operand), error = TRUE)
  }
  # operand has wrong dtype (boolean not allowed)
  check(vt("pred", c(2L, 3L)))
})

test_that("infer_types_float_uni errors", {
  check <- function(operand) {
    expect_snapshot(infer_types_float_uni(operand), error = TRUE)
  }
  # operand has wrong dtype
  check(vt("i32", c(2L, 3L)))
})

test_that("infer_types_integer_uni errors", {
  check <- function(operand) {
    expect_snapshot(infer_types_integer_uni(operand), error = TRUE)
  }
  # operand has wrong dtype (float not allowed)
  check(vt("f32", c(2L, 3L)))
})

test_that("infer_types_integerish_uni errors", {
  check <- function(operand) {
    expect_snapshot(infer_types_integerish_uni(operand), error = TRUE)
  }
  # operand has wrong dtype (float not allowed)
  check(vt("f32", c(2L, 3L)))
})

# ---- dynamic axis sizes ----------------------------------------------------

test_that("elementwise: a dynamic operand meets a static one", {
  expect_equal(
    inferred(function() {
      hlo_add(dyn_input("a", "f32", N), dyn_input("b", "f32", N))
    }),
    "tensor<?xf32>"
  )
  # Refinement: the known side wins, so everything downstream stays static.
  expect_equal(
    inferred(function() {
      hlo_add(dyn_input("a", "f32", N), dyn_input("b", "f32", 3L))
    }),
    "tensor<3xf32>"
  )
  expect_equal(
    inferred(function() {
      hlo_multiply(
        dyn_input("a", "f32", c(N, 3L)),
        dyn_input("b", "f32", c(2L, N))
      )
    }),
    "tensor<2x3xf32>"
  )
})

test_that("elementwise: a definite size clash is still an error", {
  local_func()
  expect_error(
    hlo_add(dyn_input("a", "f32", 3L), dyn_input("b", "f32", 4L)),
    "same tensor type"
  )
})

test_that("elementwise chain", {
  skip_if_no_refine()
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      b <- dyn_input("b", "f32", shapes[[2L]])
      hlo_multiply(hlo_add(a, b), a)
    },
    dyn_shapes = list(N, N),
    runs = list(
      list(shapes = list(3L, 3L), args = list(c(1, 2, 3), c(10, 20, 30))),
      list(shapes = list(5L, 5L), args = list(1:5 + 0, rep(2, 5)))
    )
  )
})

test_that("a dynamic operand mixed with a static one", {
  skip_if_no_refine()
  # The second argument's shape is static in the program, so refinement only
  # has the first to resolve -- and our inference already refined the result to
  # the static shape.
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      b <- dyn_input("b", "f32", shapes[[2L]])
      hlo_add(a, b)
    },
    dyn_shapes = list(N, 4L),
    runs = list(list(shapes = list(4L, 4L), args = list(1:4 + 0, rep(10, 4))))
  )
})
