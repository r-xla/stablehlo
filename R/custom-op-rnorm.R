hlo_rnorm <- function(
  shape,
  mu = 0,
  sigma = 1,
  dtype = "f64",
  rng_algorithm = "THREE_FRY",
  initial_state = c(1L, 2L)
) {
  n <- as.integer(2 * prod(shape))
  n_half <- as.integer(ceiling(n / 2))

  U1 <- hlo_runif(
    shape = c(n_half, 1L),
    dtype = "f64",
    rng_algorithm = rng_algorithm,
    initial_state = initial_state
  )
  U2 <- hlo_runif(
    shape = c(n_half, 1L),
    dtype = "f64",
    rng_algorithm = rng_algorithm,
    initial_state = as.integer(initial_state + 1L)
  )

  local_func()
  res1 <- hlo_log(
    hlo_input("U1", dtype = dtype, shape = c(n_half, 1L))
  )
  f1 <- hlo_return(res1)
  program1 <- pjrt::pjrt_program(repr(f1))
  executable1 <- pjrt::pjrt_compile(program1)
  log_U1 <- pjrt::pjrt_execute(
    executable1,
    U1
  )

  local_func()
  res2 <- hlo_multiply(
    hlo_input("log_U1", dtype = dtype, shape = c(n_half, 1L)),
    hlo_input("factor", dtype = dtype, shape = c(n_half, 1L))
  )
  f2 <- hlo_return(res2)
  program2 <- pjrt::pjrt_program(repr(f2))
  executable2 <- pjrt::pjrt_compile(program2)
  R <- pjrt::pjrt_execute(
    executable2,
    log_U1,
    pjrt::pjrt_buffer(
      array(rep(-2, n_half), dim = c(n_half, 1L)),
      dtype = "f64"
    )
  )

  Theta <- pjrt::pjrt_execute(
    executable2,
    U2,
    pjrt::pjrt_buffer(
      array(rep(2 * pi, n_half), dim = c(n_half, 1L)),
      dtype = "f64"
    )
  )

  local_func()
  res3 <- hlo_sqrt(
    hlo_input("R", dtype = dtype, shape = c(n_half, 1L))
  )
  f3 <- hlo_return(res3)
  program3 <- pjrt::pjrt_program(repr(f3))
  executable3 <- pjrt::pjrt_compile(program3)
  sqrt_R <- pjrt::pjrt_execute(
    executable3,
    R
  )

  local_func()
  res4 <- hlo_sine(
    hlo_input("Theta", dtype = dtype, shape = c(n_half, 1L))
  )
  f4 <- hlo_return(res4)
  program4 <- pjrt::pjrt_program(repr(f4))
  executable4 <- pjrt::pjrt_compile(program4)
  sine_Theta <- pjrt::pjrt_execute(
    executable4,
    Theta
  )

  # local_func()
  # res5 <- hlo_cosine(
  #   hlo_input("Theta", dtype = dtype, shape = c(n_half, 1L))
  # )
  # f5 <- hlo_return(res5)
  # program5 <- pjrt::pjrt_program(repr(f5))
  # executable5 <- pjrt::pjrt_compile(program5)
  # cosine_Theta <- pjrt::pjrt_execute(
  #   executable5,
  #   Theta
  # )

  Z <- pjrt::pjrt_execute(
    executable2,
    sqrt_R,
    sine_Theta
  )

  # Z2 <- pjrt::pjrt_execute(
  #   executable2,
  #   sqrt_R,
  #   cosine_Theta
  # )
  #
  # local_func()
  # res6 <- hlo_concatenate(
  #   hlo_input("Z1", dtype = dtype, shape = c(n_half, 1L)),
  #   hlo_input("Z3", dtype = dtype, shape = c(n_half, 1L)),
  #   dimension = 1
  # )
  # f6 <- hlo_return(res6)
  # program6 <- pjrt::pjrt_program(repr(f6))
  # executable6 <- pjrt::pjrt_compile(program6)
  # Z <- pjrt::pjrt_execute(
  #   executable6,
  #   Z1,
  #   Z2
  # )

  Z_offset <- pjrt::pjrt_execute(
    executable2,
    Z,
    pjrt::pjrt_buffer(
      array(rep(sigma, n_half), dim = c(n_half, 1L)),
      dtype = "f64"
    )
  )

  local_func()
  res7 <- hlo_add(
    hlo_input("Z_offset", dtype = dtype, shape = c(n_half, 1L)),
    hlo_input("mu", dtype = dtype, shape = c(n_half, 1L))
  )
  f7 <- hlo_return(res7)
  program7 <- pjrt::pjrt_program(repr(f7))
  executable7 <- pjrt::pjrt_compile(program7)
  N <- pjrt::pjrt_execute(
    executable7,
    Z_offset,
    pjrt::pjrt_buffer(
      array(rep(mu, n_half), dim = c(n_half, 1L)),
      dtype = "f64"
    )
  )

  local_func()
  res8 <- hlo_reshape(
    hlo_input("N", dtype = dtype, shape = c(n_half, 1L)),
    shape_out = shape
  )
  f8 <- hlo_return(res8)
  program8 <- pjrt::pjrt_program(repr(f8))
  executable8 <- pjrt::pjrt_compile(program8)
  N_out <- pjrt::pjrt_execute(
    executable8,
    N
  )

  N_out
}

debugonce(hlo_rnorm)
X <- hlo_rnorm(
  shape = c(20L, 150L),
  mu = 10,
  sigma = 5,
  initial_state = c(175L, 10L)
) |>
  as_array()


mean(X)
sd(X)
