if (requireNamespace("pjrt", quietly = TRUE)) {
  library("pjrt")
}

generate_test_data <- function(dtype, dimension, non_negative = FALSE) {
  if (dtype == "pred" || dtype == "i1") {
    sample(c(TRUE, FALSE), size = prod(dimension), replace = TRUE)
  } else if (dtype %in% c("i8", "i16", "i32", "i64")) {
    # Signed integers
    sample(-10:10, size = prod(dimension), replace = TRUE)
  } else if (dtype %in% c("ui8", "ui16", "ui32", "ui64")) {
    # Unsigned integers
    sample(0:20, size = prod(dimension), replace = TRUE)
  } else if (dtype %in% c("f32", "f64")) {
    # Floating point
    if (!non_negative) {
      rnorm(prod(dimension), mean = 0, sd = 1)
    } else {
      rchisq(prod(dimension), df = 1)
    }
  } else {
    # Default case
    if (!non_negative) {
      rnorm(prod(dimension), mean = 0, sd = 1)
    } else {
      rchisq(prod(dimension), df = 1)
    }
  }
}

hlo_test_uni <- function(
  hlo_fn,
  test_func,
  non_negative = FALSE,
  dimension = NULL,
  test_data = NULL,
  dtype = "f64",
  tol = 1e-5,
  snapshot = TRUE
) {
  make_fn <- function() {
    func <- local_func()
    if (is.null(dimension)) {
      len <- min(rgeom(1, .3) + 1, 4)
      dimension <- pmin(as.integer(rgeom(len, .2) + 1), rep(3, len))
    }
    x <- hlo_input("x", dtype, shape = dimension)
    y <- hlo_fn(x)
    func <- hlo_return(y)
    list(
      dimension = dimension,
      func = func
    )
  }

  if (snapshot) {
    withr::with_seed(1, {
      f <- make_fn()$func
      testthat::expect_snapshot(repr(f))
    })
  }

  res <- make_fn()
  func <- res$func
  dimension <- res$dimension

  testthat::skip_if_not_installed("pjrt")
  program <- pjrt::pjrt_program(repr(func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt::pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  test_data <- if (is.null(test_data)) {
    generate_test_data(dtype, dimension, non_negative)
  }

  x <- array(test_data, dim = dimension)
  x_buf <- pjrt::pjrt_buffer(x, dtype = dtype)
  out_buf <- pjrt::pjrt_execute(executable, x_buf)
  expect_class(out_buf, "PJRTBuffer")
  out <- as_array(out_buf)
  testthat::expect_equal(out, test_func(x), tolerance = tol)
}

hlo_test_biv <- function(
  hlo_fn,
  test_func,
  non_negative = FALSE,
  dimension = NULL,
  dtype = "f64",
  lhs = NULL,
  rhs = NULL,
  tol = 1e-5,
  args = list(),
  snapshot = TRUE
) {
  if (is.null(dtype)) {
    dtype <- "f64"
  }
  make_fn <- function() {
    if (is.null(dimension)) {
      len <- min(rgeom(1, .3) + 1, 4)
      dimension <- pmin(as.integer(rgeom(len, .2) + 1), rep(3, len))
    }
    local_func()
    x <- hlo_input("x", dtype, shape = dimension)
    y <- hlo_input("y", dtype, shape = dimension)
    z <- do.call(hlo_fn, c(list(x, y), args))
    list(
      dimension = dimension,
      f = hlo_return(z)
    )
  }

  if (snapshot) {
    withr::with_seed(1, {
      f <- make_fn()$f
      testthat::expect_snapshot(repr(f))
    })
  }

  res <- make_fn()
  func <- res$f
  dimension <- res$dimension

  testthat::skip_if_not_installed("pjrt")
  program <- pjrt::pjrt_program(repr(func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt::pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  if (is.null(lhs)) {
    lhs <- generate_test_data(dtype, dimension, non_negative)
  }

  if (is.null(rhs)) {
    rhs <- generate_test_data(dtype, dimension, non_negative)
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
