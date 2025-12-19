test_that("print_tensor with header works on CPU", {
  skip_if_not_installed("pjrt")

  local_func()
  x <- hlo_input("x", "f32", shape = c(2, 2))
  hlo_custom_call(
    x,
    call_target_name = "print_tensor",
    backend_config = list(print_header = "MyTensor")
  )
  f <- hlo_return(x)
  expect_snapshot(repr(f))

  program <- pjrt::pjrt_program(repr(f))
  exec <- pjrt::pjrt_compile(program)

  buf <- pjrt::pjrt_buffer(matrix(1:4, nrow = 2, ncol = 2), dtype = "f32")

  expect_snapshot({
    out <- pjrt::pjrt_execute(exec, buf)
  })

  expect_equal(out, buf)
})
