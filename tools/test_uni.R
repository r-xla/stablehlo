hlo_test_uni <- function(hlo_func,
                         test_func,
                         non_negative = FALSE,
                         dimension = NULL,
                         test_data = NULL,
                         tol = NULL) {
  local_reset_id_gen()
  if (is.null(dimension)) {
    len <- rgeom(1, .3) + 1
    dimension <- as.integer(rgeom(len, .2) + 1)
  }

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
}

# test_that("basic test", {hlo_test_uni(hlo_abs, abs, tol = 1e-6)})
