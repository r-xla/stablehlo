if (requireNamespace("pjrt", quietly = TRUE)) {
  library("pjrt")
}

# Helper to create ValueType inputs for inference function tests
vt <- function(dtype, shape) {
  ValueType(dtype, shape = shape)
}

# Helper to create Constant values for inference function tests
cnst <- function(value, dtype, shape) {
  Constant(
    value,
    TensorType(dtype = as_dtype(dtype), shape = Shape(shape))
  )
}

# Helper to create scalar Constant values for inference function tests
scnst <- function(value, dtype) {
  cnst(value, dtype, shape = integer())
}

row_major_array <- function(data, dim) {
  arr <- array(data, dim = rev(dim))
  aperm(arr, perm = rev(seq_along(dim)))
}

hlo_test_uni <- function(
  hlo_fn,
  test_func,
  non_negative = FALSE,
  dimension = NULL,
  test_data = NULL,
  dtype = "f32",
  tol = 1e-4,
  snapshot = TRUE
) {
  make_fn <- function(dtype, dim = NULL) {
    func <- local_func()
    if (is.null(dim)) {
      len <- min(rgeom(1, .3) + 1, 4)
      dim <- pmin(as.integer(rgeom(len, .2) + 1), rep(3, len))
    }
    x <- hlo_input("x", dtype, shape = dim)
    y <- hlo_fn(x)
    func <- hlo_return(y)
    list(
      dimension = dim,
      func = func
    )
  }

  if (snapshot) {
    withr::with_seed(1, {
      f <- make_fn(sample(dtype, 1))$func
      testthat::expect_snapshot(repr(f))
    })
  }

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
    test_data <- generate_test_data(dimension, dtype, non_negative)
  }

  x <- array(test_data, dim = dimension)
  x_buf <- pjrt::pjrt_buffer(x, dtype = dtype)
  out_buf <- pjrt::pjrt_execute(executable, x_buf)
  expect_class(out_buf, "PJRTBuffer")
  out <- pjrt::as_array(out_buf)
  testthat::expect_equal(out, test_func(x), tolerance = tol)
}

hlo_test_biv <- function(
  hlo_fn,
  test_func,
  non_negative = list(FALSE, FALSE),
  dimension = NULL,
  dtypes = "f32",
  lhs = NULL,
  rhs = NULL,
  tol = 1e-4,
  snapshot = TRUE,
  args = list()
) {
  make_fn <- function(dtype, dim = NULL) {
    local_func()
    if (is.null(dim)) {
      len <- min(rgeom(1, .3) + 1, 4)
      dim <- pmin(as.integer(rgeom(len, .2) + 1), rep(3, len))
    }
    x <- hlo_input("x", dtype, shape = dim)
    y <- hlo_input("y", dtype, shape = dim)
    z <- do.call(hlo_fn, c(list(x, y), args))
    func <- hlo_return(z)
    list(
      dimension = dim,
      func = func
    )
  }

  if (snapshot) {
    withr::with_seed(1, {
      f <- make_fn(sample(dtypes, 1))$func
      testthat::expect_snapshot(repr(f))
    })
  }

  dtype <- sample(dtypes, 1)
  res <- make_fn(dtype, dim = dimension)
  func <- res$func
  dimension <- res$dimension

  testthat::skip_if_not_installed("pjrt")
  program <- pjrt::pjrt_program(repr(func))
  expect_class(program, "PJRTProgram")

  executable <- pjrt::pjrt_compile(program)
  expect_class(executable, "PJRTLoadedExecutable")

  if (length(non_negative) < 2) {
    non_negative <- rep(non_negative, 2)
  }

  if (is.null(rhs)) {
    rhs <- generate_test_data(
      dimension,
      dtype,
      non_negative = non_negative[[2]]
    )
  }

  if (is.null(lhs)) {
    lhs <- generate_test_data(
      dimension,
      dtype,
      non_negative = non_negative[[1]]
    )
  }

  x <- if (length(dim)) {
    array(lhs, dim = dimension)
  } else {
    lhs
  }
  y <- if (length(dim)) {
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

generate_test_data <- function(dimension, dtype = "f64", non_negative = FALSE) {
  if (dtype == "pred") {
    sample(c(TRUE, FALSE), size = prod(dimension), replace = TRUE)
  } else if (dtype %in% c("ui8", "ui16", "ui32", "ui64")) {
    # Signed integers
    sample(0:20, size = prod(dimension), replace = TRUE)
  } else if (dtype %in% c("i8", "i16", "i32", "i64")) {
    test_data <- as.integer(rgeom(prod(dimension), .5))
    if (!non_negative) {
      test_data <- as.integer((-1)^rbinom(prod(dimension), 1, .5) * test_data)
    }
    test_data
  } else {
    if (!non_negative) {
      rnorm(prod(dimension), mean = 0, sd = 1)
    } else {
      rchisq(prod(dimension), df = 1)
    }
  }
}
