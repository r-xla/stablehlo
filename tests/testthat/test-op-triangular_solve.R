test_that("errors", {
  check <- function(a, b, left_side = TRUE, transpose_a = "NO_TRANSPOSE") {
    expect_snapshot(
      infer_types_triangular_solve(
        a,
        b,
        left_side = scnst(left_side, "pred"),
        lower = scnst(TRUE, "pred"),
        unit_diagonal = scnst(FALSE, "pred"),
        transpose_a = transpose_a
      ),
      error = TRUE
    )
  }
  # (C2) rank < 2
  check(vt("f32", 3L), vt("f32", 3L))
  # (C2) different rank
  check(vt("f32", c(3L, 3L)), vt("f32", c(2L, 3L, 3L)))
  # (C3) not square
  check(vt("f32", c(3L, 4L)), vt("f32", c(3L, 2L)))
  # batch dimensions mismatch
  check(vt("f32", c(2L, 3L, 3L)), vt("f32", c(4L, 3L, 2L)))
  # dimension mismatch
  check(vt("f32", c(3L, 3L)), vt("f32", c(4L, 2L)))
  # invalid transpose_a
  check(vt("f32", c(3L, 3L)), vt("f32", c(3L, 2L)), transpose_a = "INVALID")
})

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
