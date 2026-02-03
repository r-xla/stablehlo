test_that("errors", {
  # (C2) duplicate dimensions
  expect_snapshot(
    infer_types_reverse(
      vt("f32", c(2L, 3L)),
      dimensions = cnst(c(0L, 0L), "i64", 2L)
    ),
    error = TRUE
  )
  # empty dimensions
  expect_snapshot(
    infer_types_reverse(
      vt("f32", c(2L, 3L)),
      dimensions = cnst(integer(), "i64", 0L)
    ),
    error = TRUE
  )
  # (C3) dimension out of bounds
  expect_snapshot(
    infer_types_reverse(
      vt("f32", c(2L, 3L)),
      dimensions = cnst(5L, "i64", 1L)
    ),
    error = TRUE
  )
})

test_that("basic tests", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 3L, 2L))
  y <- hlo_reverse(
    x,
    dimensions = c(1L, 0L)
  )
  f <- hlo_return(y)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt_program(repr(f))
  exec <- pjrt_compile(program)

  input <- array(as.double(1:12), dim = c(2, 3, 2))
  expected <- input[, rev(seq_len(dim(input)[[2]])), ]
  expected <- expected[rev(seq_len(dim(input)[[1]])), , ]

  output <- pjrt_execute(exec, pjrt_buffer(input))
  expect_equal(as_array(output), expected)
})
