hlo_test_biv <- function(hlo_func,
                         test_func,
                         non_negative = FALSE,
                         dimension = NULL,
                         lhs = NULL,
                         rhs = NULL,
                         tol = NULL) {

  # checkmate::assertFunction(hlo_func)
  # checkmate::assertFunction(test_func)
  # checkmate::assertInteger(dimension, min.len = 1, lower = 1, any.missing = FALSE)
  # checkmate::assertFlag(non_negative)
  # checkmate::assertNumeric(lhs,
  #                          len = prod(dimension),
  #                          lower = ifelse(non_negative, 0, -Inf),
  #                          any.missing = FALSE,
  #                          null.ok = TRUE)
  # checkmate::assertNumeric(rhs,
  #                          len = prod(dimension),
  #                          lower = ifelse(non_negative, 0, -Inf),
  #                          any.missing = FALSE,
  #                          null.ok = TRUE)
  # checkmate::assertNumeric(tol, len = 1, lower = .Machine$double.eps, null.ok = TRUE, any.missing = FALSE)

  local_reset_id_gen()
  if (is.null(dimension)) {
    len <- rgeom(1, .3) + 1
    dimension <- as.integer(rgeom(len, .2) + 1)
  }
  x <- hlo_input("x", "f32", shape = dimension, "main")
  y <- hlo_input("y", "f32", shape = dimension, "main")
  z <- hlo_func(x, y)
  func <- hlo_return(z)
  expect_snapshot(repr(func))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  if (is.null(lhs)) {
    if (!non_negative) {
      lhs <- rnorm(prod(dimension), mean = 0, sd = 1)
    } else {
      lhs <- rchisq(prod(dimension), df = 1)
    }
  }

  if (is.null(rhs)) {
    if (!non_negative) {
      rhs <- rnorm(prod(dimension), mean = 0, sd = 1)
    } else {
      rhs <- rchisq(prod(dimension), df = 1)
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

# test_that("basic test", {hlo_test_biv(hlo_add, `+`, tol = 1e-6)})
