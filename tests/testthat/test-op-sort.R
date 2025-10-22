test_that("basic tests", {
  local_func()
  comp <- hlo_compare(
    hlo_input("lhs", "i32", shape = c(1, 3)),
    hlo_input("rhs", "i32", shape = c(1, 3)),
    comparison_direction = "GT",
    compare_type = "SIGNED"
  )
  comp.func <- hlo_return(comp)

  func <- local_func()
  x1 <- hlo_input("x1", "i32", shape = c(2L, 3L))
  x2 <- hlo_input("x2", "i32", shape = c(2L, 3L))
  y <- hlo_sort(
    x1,
    x2,
    dimension = 0L, # comply with zero-indexing
    is_stable = TRUE,
    comparator = comp.func
  )
  f <- hlo_return(y[[1]])
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  x1 <- array(1L:3L, dim = c(3, 1))
  x2 <- array(1L:6L, dim = c(3, 2))
  x3 <- array(1L:9L, dim = c(3, 3))
  expected <- array(cbind(x1, x2, x3), dim = c(3, 6))

  output <- pjrt_execute(
    exec,
    pjrt_buffer(x1),
    pjrt_buffer(x2),
    pjrt_buffer(x3)
  )
  expect_equal(as_array(output), expected, tolerance = 1e-3)
})
