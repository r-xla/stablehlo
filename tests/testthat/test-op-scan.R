test_that("forward scan computes cumsum on 1-D input", {
  func <- local_func()
  x <- hlo_input("x", "f32", shape = 5L)
  z <- hlo_scalar(0, dtype = "f32")
  y <- hlo_scan(
    inputs = list(x),
    init = z,
    body = function(carry, elem) {
      g <- hlo_add(carry, elem)
      list(g, g)
    },
    dim = 0L
  )
  func <- hlo_return(y)

  skip_if_not_installed("pjrt")
  exec <- pjrt_compile(pjrt_program(repr(func)))
  res <- pjrt_execute(exec, pjrt_buffer(c(1, 2, 3, 4, 5), dtype = "f32"))
  expect_equal(as.numeric(pjrt::as_array(res)), c(1, 3, 6, 10, 15))
})

test_that("reverse scan computes suffix-sum on 1-D input", {
  func <- local_func()
  x <- hlo_input("x", "f32", shape = 5L)
  z <- hlo_scalar(0, dtype = "f32")
  y <- hlo_scan(
    inputs = list(x),
    init = z,
    body = function(carry, elem) {
      g <- hlo_add(carry, elem)
      list(g, g)
    },
    dim = 0L,
    reverse = TRUE
  )
  func <- hlo_return(y)

  skip_if_not_installed("pjrt")
  exec <- pjrt_compile(pjrt_program(repr(func)))
  res <- pjrt_execute(exec, pjrt_buffer(c(1, 2, 3, 4, 5), dtype = "f32"))
  # [1+2+3+4+5, 2+3+4+5, 3+4+5, 4+5, 5] = [15, 14, 12, 9, 5].
  expect_equal(as.numeric(pjrt::as_array(res)), c(15, 14, 12, 9, 5))
})

test_that("multi-input reverse scan computes the linear recurrence used by cumprod's gradient", {
  func <- local_func()
  a <- hlo_input("a", "f32", shape = 4L)
  b <- hlo_input("b", "f32", shape = 4L)
  z <- hlo_scalar(0, dtype = "f32")
  G <- hlo_scan(
    inputs = list(a, b),
    init = z,
    body = function(carry, ai, bi) {
      g <- hlo_add(ai, hlo_multiply(bi, carry))
      list(g, g)
    },
    dim = 0L,
    reverse = TRUE
  )
  func <- hlo_return(G)

  skip_if_not_installed("pjrt")
  exec <- pjrt_compile(pjrt_program(repr(func)))
  # x = [1, 2, 3, 4], grad_y = [1, 1, 1, 1] for cumprod => a = grad_y, b = x_next.
  # G_4 = 1; G_3 = 1 + 4*1 = 5; G_2 = 1 + 3*5 = 16; G_1 = 1 + 2*16 = 33.
  res <- pjrt_execute(
    exec,
    pjrt_buffer(c(1, 1, 1, 1), dtype = "f32"),
    pjrt_buffer(c(2, 3, 4, 0), dtype = "f32")
  )
  expect_equal(as.numeric(pjrt::as_array(res)), c(33, 16, 5, 1))
})

test_that("scan over a non-leading dim of a 2-D input scans each row independently", {
  func <- local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 4L))
  # carry has slice shape (2,) — one element per row.
  z <- impl_hlo_constant(c(0, 0), dtype = "f32", func = .current_func(), shape = 2L)
  y <- hlo_scan(
    inputs = list(x),
    init = z,
    body = function(carry, elem) {
      g <- hlo_add(carry, elem)
      list(g, g)
    },
    dim = 1L
  )
  func <- hlo_return(y)

  skip_if_not_installed("pjrt")
  exec <- pjrt_compile(pjrt_program(repr(func)))
  x_arr <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 2)
  res <- pjrt_execute(exec, pjrt_buffer(x_arr, dtype = "f32"))
  expected <- t(apply(x_arr, 1, cumsum))
  expect_equal(pjrt::as_array(res), expected)
})

test_that("inputs with different shapes are rejected", {
  local_func()
  a <- hlo_input("a", "f32", shape = 4L)
  b <- hlo_input("b", "f32", shape = 5L)
  z <- hlo_scalar(0, dtype = "f32")
  expect_snapshot(
    hlo_scan(
      inputs = list(a, b),
      init = z,
      body = function(carry, ai, bi) list(carry, carry),
      dim = 0L
    ),
    error = TRUE
  )
})

test_that("rank-0 input is rejected", {
  local_func()
  a <- hlo_input("a", "f32")
  z <- hlo_scalar(0, dtype = "f32")
  expect_snapshot(
    hlo_scan(
      inputs = list(a),
      init = z,
      body = function(carry, elem) list(carry, carry),
      dim = 0L
    ),
    error = TRUE
  )
})

test_that("body returning the wrong out_elem shape is rejected", {
  local_func()
  a <- hlo_input("a", "f32", shape = 4L)
  z <- hlo_scalar(0, dtype = "f32")
  expect_snapshot(
    hlo_scan(
      inputs = list(a),
      init = z,
      # out_elem must be scalar (1-D input minus dim => rank 0); returning
      # `a` (shape (4,)) is wrong.
      body = function(carry, elem) list(carry, a),
      dim = 0L
    ),
    error = TRUE
  )
})
