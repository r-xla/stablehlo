test_that("basic triangular_solve", {
  func <- local_func()
  a <- hlo_input("a", "f32", shape = c(3L, 3L))
  b <- hlo_input("b", "f32", shape = c(3L, 3L))
  y <- hlo_triangular_solve(
    a,
    b,
    left_side = TRUE,
    lower = TRUE,
    unit_diagonal = FALSE,
    transpose_a = "NO_TRANSPOSE"
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  # Lower triangular matrix
  a_mat <- matrix(
    c(
      1,
      0,
      0,
      2,
      4,
      0,
      3,
      5,
      6
    ),
    nrow = 3,
    byrow = TRUE
  )

  # b = a * expected_solution, where expected_solution is identity * 2
  expected_solution <- diag(2, 3, 3)
  b_mat <- a_mat %*% expected_solution

  output <- pjrt_execute(
    exec,
    pjrt_buffer(a_mat, dtype = "f32"),
    pjrt_buffer(b_mat, dtype = "f32")
  )
  expect_equal(
    output,
    pjrt_buffer(expected_solution, dtype = "f32"),
    tolerance = 1e-5
  )
})

test_that("right_side solve", {
  func <- local_func()
  a <- hlo_input("a", "f32", shape = c(3L, 3L))
  b <- hlo_input("b", "f32", shape = c(3L, 3L))
  y <- hlo_triangular_solve(
    a,
    b,
    left_side = FALSE,
    lower = FALSE,
    unit_diagonal = FALSE,
    transpose_a = "NO_TRANSPOSE"
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))
})

test_that("batched triangular_solve", {
  func <- local_func()
  a <- hlo_input("a", "f32", shape = c(2L, 3L, 3L))
  b <- hlo_input("b", "f32", shape = c(2L, 3L, 4L))
  y <- hlo_triangular_solve(
    a,
    b,
    left_side = TRUE,
    lower = TRUE,
    unit_diagonal = FALSE,
    transpose_a = "NO_TRANSPOSE"
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))
})

test_that("transpose_a = TRANSPOSE", {
  func <- local_func()
  a <- hlo_input("a", "f32", shape = c(3L, 3L))
  b <- hlo_input("b", "f32", shape = c(3L, 3L))
  y <- hlo_triangular_solve(
    a,
    b,
    left_side = TRUE,
    lower = TRUE,
    unit_diagonal = FALSE,
    transpose_a = "TRANSPOSE"
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))
})

test_that("transpose_a = ADJOINT", {
  func <- local_func()
  a <- hlo_input("a", "f32", shape = c(3L, 3L))
  b <- hlo_input("b", "f32", shape = c(3L, 3L))
  y <- hlo_triangular_solve(
    a,
    b,
    left_side = TRUE,
    lower = TRUE,
    unit_diagonal = FALSE,
    transpose_a = "ADJOINT"
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))
})

test_that("error when a is not square", {
  func <- local_func()
  a <- hlo_input("a", "f32", shape = c(3L, 4L))
  b <- hlo_input("b", "f32", shape = c(3L, 3L))
  expect_error(
    hlo_triangular_solve(
      a,
      b,
      left_side = TRUE,
      lower = TRUE,
      unit_diagonal = FALSE,
      transpose_a = "NO_TRANSPOSE"
    ),
    "square matrix"
  )
})

test_that("error when ranks don't match", {
  func <- local_func()
  a <- hlo_input("a", "f32", shape = c(3L, 3L))
  b <- hlo_input("b", "f32", shape = c(2L, 3L, 3L))
  expect_error(
    hlo_triangular_solve(
      a,
      b,
      left_side = TRUE,
      lower = TRUE,
      unit_diagonal = FALSE,
      transpose_a = "NO_TRANSPOSE"
    ),
    "same rank"
  )
})

test_that("error when batch dimensions don't match", {
  func <- local_func()
  a <- hlo_input("a", "f32", shape = c(2L, 3L, 3L))
  b <- hlo_input("b", "f32", shape = c(4L, 3L, 3L))
  expect_error(
    hlo_triangular_solve(
      a,
      b,
      left_side = TRUE,
      lower = TRUE,
      unit_diagonal = FALSE,
      transpose_a = "NO_TRANSPOSE"
    ),
    "Batch dimensions"
  )
})

test_that("error when dimension mismatch with left_side = TRUE", {
  func <- local_func()
  a <- hlo_input("a", "f32", shape = c(3L, 3L))
  b <- hlo_input("b", "f32", shape = c(4L, 3L))
  expect_error(
    hlo_triangular_solve(
      a,
      b,
      left_side = TRUE,
      lower = TRUE,
      unit_diagonal = FALSE,
      transpose_a = "NO_TRANSPOSE"
    ),
    "Dimension mismatch"
  )
})

test_that("error when dimension mismatch with left_side = FALSE", {
  func <- local_func()
  a <- hlo_input("a", "f32", shape = c(3L, 3L))
  b <- hlo_input("b", "f32", shape = c(3L, 4L))
  expect_error(
    hlo_triangular_solve(
      a,
      b,
      left_side = FALSE,
      lower = TRUE,
      unit_diagonal = FALSE,
      transpose_a = "NO_TRANSPOSE"
    ),
    "Dimension mismatch"
  )
})

test_that("error for invalid transpose_a", {
  func <- local_func()
  a <- hlo_input("a", "f32", shape = c(3L, 3L))
  b <- hlo_input("b", "f32", shape = c(3L, 3L))
  expect_error(
    hlo_triangular_solve(
      a,
      b,
      left_side = TRUE,
      lower = TRUE,
      unit_diagonal = FALSE,
      transpose_a = "INVALID"
    ),
    "must be one of"
  )
})
