test_that("matmul", {
  local_reset_id_gen()
  lhs <- hlo_input("lhs", "f32", shape = c(5, 4), func_id = "main")
  rhs <- hlo_input("rhs", "f32", shape = c(4, 3))
  z <- hlo_dot_general(
    lhs,
    rhs,
    contracting_dims = list(1L, 0L)
  )
  f <- hlo_return(z)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")

  pjrt_program <- pjrt_program(repr(f))
  exec <- pjrt_compile(pjrt_program)

  expect_equal(
    pjrt_execute(
      exec,
      pjrt_buffer(array(as.double(1:20), dim = c(5, 4))),
      pjrt_buffer(array(as.double(1:12), dim = c(4, 3)))
    ),
    pjrt_buffer(
      array(as.double(1:20), dim = c(5, 4)) %*%
        array(as.double(1:12), dim = c(4, 3))
    )
  )
})

test_that("batching_dims", {
  local_reset_id_gen()
  skip_if_not_installed("pjrt")
  lhs <- hlo_input("lhs", "f32", shape = c(1, 5, 4), func_id = "main")
  rhs <- hlo_input("rhs", "f32", shape = c(4, 3, 1L))
  z <- hlo_dot_general(
    lhs,
    rhs,
    contracting_dims = list(2L, 0L),
    batching_dims = list(0L, 2L)
  )
  f <- hlo_return(z)
  expect_snapshot(repr(f))

  x1 <- array(as.double(1:20), dim = c(1, 5, 4))
  x2 <- array(as.double(1:12), dim = c(4, 3, 1L))
  x1b <- pjrt_buffer(x1)
  x2b <- pjrt_buffer(x2)

  pjrt_program <- pjrt_program(repr(f))
  exec <- pjrt_compile(pjrt_program)
  dim(x1) <- c(5, 4)
  dim(x2) <- c(4, 3)
  x <- x1 %*% x2
  dim(x) <- c(1, 5, 3)

  expect_equal(
    pjrt_execute(exec, x1b, x2b),
    pjrt_buffer(x)
  )
})
