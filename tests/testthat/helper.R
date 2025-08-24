library("testthat")
library("checkmate")

if (requireNamespace("pjrt", quietly = TRUE)) {
  library("pjrt")
}


hlo_test_uni <- function(hlo_func,
                            test_func,
                            dimension,
                            non_negative = FALSE,
                            test_data = NULL,
                            tol = NULL) {

  checkmate::assertFunction(hlo_func)
  checkmate::assertFunction(test_func)
  checkmate::assertInteger(dimension, min.len = 1, lower = 1, any.missing = FALSE)
  checkmate::assertFlag(non_negative)
  checkmate::assertNumeric(test_data,
                           len = prod(dimension),
                           lower = ifelse(non_negative, 0, -Inf),
                           any.missing = FALSE,
                           null.ok = TRUE)
  checkmate::assertNumeric(tol, len = 1, lower = .Machine$double.eps, null.ok = TRUE, any.missing = FALSE)

  test_that("basic tests", {
    local_reset_id_gen()
    x <- hlo_input("x", "f32", shape = dimension, "main")
    y <- hlo_func(x)
    func <- hlo_return(y)
    expect_snapshot(repr(func))

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
  })
}

# hlo_test_uni(hlo_abs, abs, c(2L, 2L), tol = 1e-6)
# hlo_test_uni(hlo_cbrt, function(x) x^(1/3), c(2L, 3L, 2L), non_negative = TRUE, tol = 1e-6)

hlo_test_biv <- function(hlo_func,
                         test_func,
                         dimension,
                         non_negative = FALSE,
                         lhs = NULL,
                         rhs = NULL,
                         tol = NULL) {

  checkmate::assertFunction(hlo_func)
  checkmate::assertFunction(test_func)
  checkmate::assertInteger(dimension, min.len = 1, lower = 1, any.missing = FALSE)
  checkmate::assertFlag(non_negative)
  checkmate::assertNumeric(lhs,
                           len = prod(dimension),
                           lower = ifelse(non_negative, 0, -Inf),
                           any.missing = FALSE,
                           null.ok = TRUE)
  checkmate::assertNumeric(rhs,
                           len = prod(dimension),
                           lower = ifelse(non_negative, 0, -Inf),
                           any.missing = FALSE,
                           null.ok = TRUE)
  checkmate::assertNumeric(tol, len = 1, lower = .Machine$double.eps, null.ok = TRUE, any.missing = FALSE)

  test_that("basic tests", {
    local_reset_id_gen()
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
    expect_equal(test_func(list(x, y)), as_array(out_buf), tolerance = tol)
  })
}

# add <- function(x) Reduce(`+`, x, accumulate = FALSE)
# hlo_test_biv(hlo_add, add, dimension = c(2L,2L,3L), tol = 1e-6)
