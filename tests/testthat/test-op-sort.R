test_that("basic tests", {
  local_func()
  a1 <- hlo_input("arg1", "i32", shape = integer())
  a2 <- hlo_input("arg2", "i32", shape = integer())
  a3 <- hlo_input("arg3", "i32", shape = integer())
  a4 <- hlo_input("arg4", "i32", shape = integer())
  comp <- hlo_compare(
    a1,
    a2,
    comparison_direction = "GT",
    compare_type = "SIGNED"
  )
  comp.func <- hlo_return(comp)

  f <- list()
  for (i in seq_len(2)) {
    func <- local_func()
    x1 <- hlo_input("x1", "i32", shape = c(2L, 3L))
    x2 <- hlo_input("x2", "i32", shape = c(2L, 3L))
    y <- hlo_sort(
      x1,
      x2,
      dimension = 1L,
      is_stable = TRUE,
      comparator = comp.func
    )
    f[[i]] <- hlo_return(y[[i]], func = func)
  }

  expect_snapshot(lapply(f, repr))

  skip_if_not_installed("pjrt")
  program <- lapply(f, \(x) pjrt_program(repr(x)))
  exec <- lapply(program, \(x) pjrt_compile(x))

  x1 <- array(1L:6L * 10L, dim = c(2, 3))
  x2 <- array(1L:6L * 10L + 1L, dim = c(2, 3))

  expected <- list(x1[c(2, 1), ], x2[c(2, 1), ])

  output <- lapply(exec, \(x) {
    pjrt_execute(
      x,
      pjrt_buffer(x1),
      pjrt_buffer(x2)
    )
  })
  expect_equal(lapply(output, as_array), expected, tolerance = 1e-3)
})
