test_that("basic tests", {
  # THREE_FRY (requires state size 2)
  local_func()
  init <- hlo_input("initial_state", "ui64", shape = 2L)
  debugonce(infer_types_rng_bit_generator)
  res <- hlo_rng_bit_generator(
    init,
    rng_algorithm = "THREE_FRY",
    dtype = "f64",
    shape_out = c(2L, 2L)
  )

  ### ERROR
  # something weird happens here: even output states f64, the out_buf is of type ui64. Analogue with f32 and ui32
  ###

  f_ <- hlo_return(res[[2L]])

  # skip_if_not_installed("pjrt")
  program <- pjrt::pjrt_program(repr(f_))
  executable <- pjrt::pjrt_compile(program)
  out_buf <- pjrt::pjrt_execute(
    executable,
    pjrt::pjrt_buffer(array(1:2, dim = 2L), dtype = "ui64")
  )

  local_func()
  res_3 <- hlo_convert(
    operand = hlo_input("rng", "ui64", shape = c(2L, 2L)),
    dtype = "f64"
  )
  f_c <- hlo_return(res_3)
  prog3 <- pjrt::pjrt_program(repr(f_c))
  executable3 <- pjrt::pjrt_compile(prog3)
  inter_buf <- pjrt::pjrt_execute(
    executable3,
    out_buf
  )

  local_func()
  res2 <- hlo_divide(
    hlo_input("out", "f64", shape = c(2L, 2L)),
    hlo_input("divisor", "f64", shape = c(2L, 2L))
  )

  f_r <- hlo_return(res2)
  prog <- pjrt::pjrt_program(repr(f_r))
  executable2 <- pjrt::pjrt_compile(prog)
  out2_buf <- pjrt::pjrt_execute(
    executable2,
    inter_buf,
    pjrt::pjrt_buffer(
      array((rep(2L^64 - 1L, 4)), dim = c(2L, 2L)),
      dtype = "f64"
    )
  )
})
