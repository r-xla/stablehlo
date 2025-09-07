hlo_test_uni <- function(
  hlo_func,
  test_func,
  non_negative = FALSE,
  dimension = NULL,
  test_data = NULL,
  dtype = "f64",
  tol = 1e-5
) {
  make_fn <- function(dtype) {
    if (is.null(dimension)) {
      len <- min(rgeom(1, .3) + 1, 4)
      dimension <- pmin(as.integer(rgeom(len, .2) + 1), rep(3, len))
    }
    x <- hlo_input("x", dtype, shape = dimension, "main")
    y <- hlo_func(x)
    func <- hlo_return(y)
    list(
      dimension = dimension,
      func = func
    )
  }

  withr::with_seed(1, {
    f <- make_fn(sample(dtype, 1))$func
    testthat::expect_snapshot(repr(f))
  })

  dtype <- sample(dtype, 1)
  res <- make_fn(dtype)
  func <- res$func
  dimension <- res$dimension

  testthat::skip_if_not_installed("pjrt")
  program <- pjrt::pjrt_program(repr(func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt::pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  if (is.null(test_data)) {
    if (dtype == "pred") {
      test_data <- sample(
        c(TRUE, FALSE),
        size = prod(dimension),
        replace = TRUE
      )
    } else if (dtype == "i32") {
      test_data <- as.integer(rgeom(prod(dimension), .1))
      if (!non_negative) {
        test_data <- as.integer((-1)^rbinom(prod(dimension), 1, .5) * test_data)
      }
    } else {
      if (!non_negative) {
        test_data <- rnorm(prod(dimension), mean = 0, sd = 1)
      } else {
        test_data <- rchisq(prod(dimension), df = 1)
      }
    }
  }

  x <- array(test_data, dim = dimension)
  x_buf <- pjrt::pjrt_buffer(x, dtype = dtype)
  out_buf <- pjrt::pjrt_execute(executable, x_buf)
  expect_class(out_buf, "PJRTBuffer")
  out <- pjrt::as_array(out_buf)
  testthat::expect_equal(out, test_func(x), tolerance = tol)
}

hlo_test_biv <- function(
  hlo_func,
  test_func,
  non_negative = FALSE,
  dimension = NULL,
  dtype = "f64",
  lhs = NULL,
  rhs = NULL,
  tol = 1e-5
) {
  make_fn <- function() {
    if (is.null(dimension)) {
      len <- min(rgeom(1, .3) + 1, 4)
      dimension <- pmin(as.integer(rgeom(len, .2) + 1), rep(3, len))
    }
    x <- hlo_input("x", dtype, shape = dimension, "main")
    y <- hlo_input("y", dtype, shape = dimension, "main")
    z <- hlo_func(x, y)
    list(
      dimension = dimension,
      f = hlo_return(z)
    )
  }

  withr::with_seed(1, {
    f <- make_fn()$f
    testthat::expect_snapshot(repr(f))
  })

  res <- make_fn()
  func <- res$f
  dimension <- res$dimension

  testthat::skip_if_not_installed("pjrt")
  program <- pjrt::pjrt_program(repr(func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt::pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  if (is.null(lhs)) {
    if (dtype == "pred") {
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
    if (dtype == "pred") {
      rhs <- sample(c(TRUE, FALSE), size = prod(dimension), replace = TRUE)
    } else {
      if (!non_negative) {
        rhs <- rnorm(prod(dimension), mean = 0, sd = 1)
      } else {
        rhs <- rchisq(prod(dimension), df = 1)
      }
    }
  }

  x <- if (length(dimension)) {
    array(lhs, dim = dimension)
  } else {
    lhs
  }
  y <- if (length(dimension)) {
    array(rhs, dim = dimension)
  } else {
    rhs
  }
  x_buf <- pjrt::pjrt_buffer(x, dtype = dtype)
  y_buf <- pjrt::pjrt_buffer(y, dtype = dtype)
  out_buf <- pjrt::pjrt_execute(executable, x_buf, y_buf)
  expect_class(out_buf, "PJRTBuffer")
  testthat::expect_equal(
    test_func(x, y),
    pjrt::as_array(out_buf),
    tolerance = tol
  )
}
