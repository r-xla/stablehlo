#' @include op.R hlo.R
NULL

OpRngBitGenerator <- new_Op("OpRngBitGenerator", "rng_bit_generator")

#' @rdname hlo_rng_bit_generator
#' @export
infer_types_rng_bit_generator <- function(
  initial_state,
  rng_algorithm,
  dtype,
  shape
) {
  assert_vt_is_tensor(initial_state)
  assert_vt_has_ttype(initial_state, dtype = UnsignedType(64L), ndims = 1L)

  if (!test_choice(rng_algorithm, c("DEFAULT", "THREE_FRY", "PHILOX"))) {
    cli_abort(c(
      "{.arg rng_algorithm} must be one of DEFAULT, THREE_FRY, PHILOX",
      x = "Got {.val {rng_algorithm}}."
    ))
  }

  init_shape <- shape(initial_state)
  # Validate algorithm-specific state size constraints
  algo <- as.character(rng_algorithm)
  state_size <- init_shape[[1L]]
  if (algo == "THREE_FRY") {
    if (state_size != 2L) {
      cli_abort(c(
        "THREE_FRY requires size(initial_state) = 2",
        x = "Got {.val {state_size}}."
      ))
    }
  } else if (algo == "PHILOX") {
    if (!(state_size %in% c(2L, 3L))) {
      cli_abort(c(
        "PHILOX requires size(initial_state) to be 2 or 3",
        x = "Got {.val {state_size}}."
      ))
    }
  }

  out_dtype <- as_dtype(dtype)
  assert_one_of(
    out_dtype,
    c("IntegerType", "UnsignedType", "FloatType")
  )
  out_shape <- as.integer(shape)

  # (C1)
  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = UnsignedType(64L),
        shape = Shape(init_shape)
      )
    ),
    ValueType(
      TensorType(
        dtype = out_dtype,
        shape = Shape(out_shape)
      )
    )
  ))
}

hlo_rng_bit_generator_impl <- hlo_fn(
  OpRngBitGenerator,
  infer_types_rng_bit_generator
)

#' @templateVar mnemonic rng_bit_generator
#' @template op
#' @export
hlo_rng_bit_generator <- function(
  initial_state,
  rng_algorithm = c("DEFAULT", "THREE_FRY", "PHILOX"),
  dtype,
  shape
) {
  algo <- match.arg(rng_algorithm)
  hlo_rng_bit_generator_impl(
    values = list(initial_state = initial_state),
    custom_attrs = list(
      rng_algorithm = algo,
      dtype = dtype,
      shape = as.integer(shape)
    )
  )
}

#' @export
repr.OpRngBitGenerator <- function(x, ...) {
  paste0(
    repr(x$outputs),
    " = \"stablehlo.rng_bit_generator\" ",
    "(",
    repr(x$inputs$values),
    ") {\n",
    "rng_algorithm = #stablehlo<rng_algorithm ",
    x$inputs$custom_attrs$rng_algorithm,
    ">\n}",
    ": ",
    repr(x$signature)
  )
}
