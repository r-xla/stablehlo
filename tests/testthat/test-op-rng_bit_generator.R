test_that("basic tests", {
  # THREE_FRY (requires state size 2)
  local_func()
  init <- hlo_input("initial_state", "ui64", shape = 2L)
  res <- hlo_rng_bit_generator(
    init,
    rng_algorithm = "THREE_FRY",
    dtype = "ui64",
    shape = c(2L, 2L)
  )
  f <- hlo_return(res[[1L]], res[[2L]])
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt::pjrt_program(repr(f))
  executable <- pjrt::pjrt_compile(program)
  out_buf <- pjrt::pjrt_execute(
    executable,
    pjrt::pjrt_buffer(array(1:2, dim = 2L), dtype = "ui64")
  )
  expect_equal(shape(out_buf[[1L]]), 2L)
  expect_equal(shape(out_buf[[2L]]), c(2L, 2L))

  # DEFAULT (implementation-defined state size; use 2 here)
  local_func()
  init_d <- hlo_input("initial_state", "ui64", shape = 2L)
  res_d <- hlo_rng_bit_generator(
    init_d,
    rng_algorithm = "DEFAULT",
    dtype = "ui64",
    shape = c(2L, 2L)
  )
  f_d <- hlo_return(res_d[[1L]], res_d[[2L]])
  expect_snapshot(repr(f_d))

  skip_if_not_installed("pjrt")
  program <- pjrt::pjrt_program(repr(f_d))
  executable <- pjrt::pjrt_compile(program)
  out_buf <- pjrt::pjrt_execute(
    executable,
    pjrt::pjrt_buffer(array(1:2, dim = 2L), dtype = "ui64")
  )
  expect_equal(shape(out_buf[[1L]]), 2L)
  expect_equal(shape(out_buf[[2L]]), c(2L, 2L))

  # PHILOX with state size 2
  local_func()
  init_p2 <- hlo_input("initial_state", "ui64", shape = 2L)
  res_p2 <- hlo_rng_bit_generator(
    init_p2,
    rng_algorithm = "PHILOX",
    dtype = "ui64",
    shape = c(2L, 2L)
  )
  f_p2 <- hlo_return(res_p2[[1L]], res_p2[[2L]])
  expect_snapshot(repr(f_p2))

  skip_if_not_installed("pjrt")
  program <- pjrt::pjrt_program(repr(f_p2))
  executable <- pjrt::pjrt_compile(program)
  out_buf <- pjrt::pjrt_execute(
    executable,
    pjrt::pjrt_buffer(array(1:2, dim = 2L), dtype = "ui64")
  )
  expect_equal(shape(out_buf[[1L]]), 2L)
  expect_equal(shape(out_buf[[2L]]), c(2L, 2L))

  # PHILOX with state size 3
  local_func()
  init_p3 <- hlo_input("initial_state", "ui64", shape = 3L)
  res_p3 <- hlo_rng_bit_generator(
    init_p3,
    rng_algorithm = "PHILOX",
    dtype = "ui64",
    shape = c(2L, 2L)
  )
  f_p3 <- hlo_return(res_p3[[1L]], res_p3[[2L]])
  expect_snapshot(repr(f_p3))

  skip_if_not_installed("pjrt")
  program <- pjrt::pjrt_program(repr(f_p3))
  executable <- pjrt::pjrt_compile(program)
  out_buf <- pjrt::pjrt_execute(
    executable,
    pjrt::pjrt_buffer(array(1:3, dim = 3L), dtype = "ui64")
  )
  expect_equal(shape(out_buf[[1L]]), 3L)
  expect_equal(shape(out_buf[[2L]]), c(2L, 2L))
})

test_that("errors", {
  state <- vt("ui64", 2L)
  # invalid rng_algorithm
  expect_snapshot(
    infer_types_rng_bit_generator(
      state,
      "INVALID",
      dtype = "f32",
      shape = c(3L, 2L)
    ),
    error = TRUE
  )
  # THREE_FRY requires state size = 2
  expect_snapshot(
    infer_types_rng_bit_generator(
      vt("ui64", 4L),
      "THREE_FRY",
      dtype = "f32",
      shape = c(3L, 2L)
    ),
    error = TRUE
  )
  # PHILOX requires state size = 2 or 3
  expect_snapshot(
    infer_types_rng_bit_generator(
      vt("ui64", 4L),
      "PHILOX",
      dtype = "f32",
      shape = c(3L, 2L)
    ),
    error = TRUE
  )
})
