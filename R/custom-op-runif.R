hlo_runif <- function(
  shape,
  lower = 0,
  upper = 1,
  dtype = "f64",
  rng_algorithm = "DEFAULT",
  initial_state = c(1L, 2L)
) {
  scale <- upper - lower
  dim_initial_state <- as.integer(length(initial_state))

  local_func()
  res1 <- hlo_rng_bit_generator(
    hlo_input("initial_state", "ui64", shape = dim_initial_state),
    rng_algorithm = rng_algorithm,
    dtype = "ui64",
    shape_out = shape
  )
  f1 <- hlo_return(res1[[2]])
  program1 <- pjrt::pjrt_program(repr(f1))
  executable1 <- pjrt::pjrt_compile(program1)
  out_buf1 <- pjrt::pjrt_execute(
    executable1,
    pjrt::pjrt_buffer(
      array(initial_state, dim = dim_initial_state),
      dtype = "ui64"
    )
  )

  local_func()
  res2 <- hlo_convert(
    operand = hlo_input("random_bits", "ui64", shape = shape),
    dtype = dtype
  )
  f2 <- hlo_return(res2)
  program2 <- pjrt::pjrt_program(repr(f2))
  executable2 <- pjrt::pjrt_compile(program2)
  out_buf2 <- pjrt::pjrt_execute(
    executable2,
    out_buf1
  )

  local_func()
  res3 <- hlo_divide(
    hlo_input("input", dtype = dtype, shape = shape),
    hlo_input("divisor", dtype = dtype, shape = shape)
  )
  f3 <- hlo_return(res3)
  program3 <- pjrt::pjrt_program(repr(f3))
  executable3 <- pjrt::pjrt_compile(program3)
  out_buf3 <- pjrt::pjrt_execute(
    executable3,
    out_buf2,
    pjrt::pjrt_buffer(
      array((rep(2L^64 - 1L, prod(shape))), dim = shape),
      dtype = dtype
    )
  )

  local_func()
  res4 <- hlo_multiply(
    hlo_input("input", dtype = dtype, shape = shape),
    hlo_input("scale", dtype = dtype, shape = shape)
  )
  f4 <- hlo_return(res4)
  program4 <- pjrt::pjrt_program(repr(f4))
  executable4 <- pjrt::pjrt_compile(program4)
  out_buf4 <- pjrt::pjrt_execute(
    executable4,
    out_buf3,
    pjrt::pjrt_buffer(
      array((rep(scale, prod(shape))), dim = shape),
      dtype = dtype
    )
  )

  local_func()
  res5 <- hlo_add(
    hlo_input("input", dtype = dtype, shape = shape),
    hlo_input("offset", dtype = dtype, shape = shape)
  )
  f5 <- hlo_return(res5)
  program5 <- pjrt::pjrt_program(repr(f5))
  executable5 <- pjrt::pjrt_compile(program5)
  out_buf5 <- pjrt::pjrt_execute(
    executable5,
    out_buf4,
    pjrt::pjrt_buffer(
      array((rep(lower, prod(shape))), dim = shape),
      dtype = dtype
    )
  )

  out_buf5
}

hlo_runif(c(2L, 3L), lower = -3, upper = 2, rng_algorithm = "THREE_FRY")
