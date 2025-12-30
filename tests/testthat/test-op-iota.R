test_that("basic tests", {
  local_func()
  res <- hlo_iota(iota_dimension = 0L, dtype = "i32", shape = c(3, 2))
  f <- hlo_return(res)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  program <- pjrt::pjrt_program(repr(f))
  executable <- pjrt::pjrt_compile(program)
  out_buf <- pjrt::pjrt_execute(executable)
  expected <- pjrt_buffer(rbind(
    c(0L, 0L),
    c(1L, 1L),
    c(2L, 2L)
  ))
  expect_equal(out_buf, pjrt::pjrt_buffer(expected))
})
