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
