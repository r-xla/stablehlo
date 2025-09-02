library("testthat")
library("checkmate")

if (requireNamespace("pjrt", quietly = TRUE)) {
  library("pjrt")
}

hlo_test_uni <- function(hlo_func,
                         test_func,
                         non_negative = FALSE,
                         dimension = NULL,
                         test_data = NULL,
                         tol = NULL) {
  local_reset_id_gen()
  if (is.null(dimension)) {
    len <- min(rgeom(1, .3) + 1, 4)
    dimension <- pmin(as.integer(rgeom(len, .2) + 1), rep(3, len))
  }

  x <- hlo_input("x", "f32", shape = dimension, "main")
  y <- hlo_func(x)
  func <- hlo_return(y)
  # expect_snapshot(repr(func))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  if (is.null(test_data)) {
    if (!non_negative) {
      test_data <- rnorm(prod(dimension), mean = 0, sd = 1)
    } else {
      test_data <- rchisq(prod(dimension), df = 1)
    }
  }

  x <- array(test_data, dim = dimension)
  x_buf <- pjrt_buffer(x)
  out_buf <- pjrt_execute(executable, x_buf)
  expect_class(out_buf, "PJRTBuffer")
  out <- as_array(out_buf)
  expect_equal(out, test_func(x), tolerance = tol)
}

hlo_test_biv <- function(hlo_func,
                         test_func,
                         non_negative = FALSE,
                         dimension = NULL,
                         type = NULL,
                         lhs = NULL,
                         rhs = NULL,
                         tol = NULL) {
  local_reset_id_gen()
  if (is.null(dimension)) {
    len <- min(rgeom(1, .3) + 1, 4)
    dimension <- pmin(as.integer(rgeom(len, .2) + 1), rep(3, len))
  }
  if (is.null(type)) {
    type <- "f32"
  }
  x <- hlo_input("x", type, shape = dimension, "main")
  y <- hlo_input("y", type, shape = dimension, "main")
  z <- hlo_func(x, y)
  func <- hlo_return(z)
  # expect_snapshot(repr(func))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  if (is.null(lhs)) {
    if (type == "pred") {
      lhs <- sample(c(TRUE, FALSE), size = prod(dimension), replace = TRUE)
    } else {
      if (!non_negative) {
        lhs <- rnorm(prod(dimension), mean = 0, sd = 1)
      } else {
        lhs <- rchisq(prod(dimension), df = 1)
      }
    }
  }

  if (is.null(rhs)) {
    if (type == "pred") {
      rhs <- sample(c(TRUE, FALSE), size = prod(dimension), replace = TRUE)
    } else {
      if (!non_negative) {
        rhs <- rnorm(prod(dimension), mean = 0, sd = 1)
      } else {
        rhs <- rchisq(prod(dimension), df = 1)
      }
    }
  }

  x <- array(lhs, dim = dimension)
  y <- array(rhs, dim = dimension)
  x_buf <- pjrt_buffer(x)
  y_buf <- pjrt_buffer(y)
  out_buf <- pjrt_execute(executable, x_buf, y_buf)
  expect_class(out_buf, "PJRTBuffer")
  expect_equal(test_func(x, y), as_array(out_buf), tolerance = tol)
}
