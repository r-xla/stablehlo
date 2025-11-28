#' @include op.R hlo.R
NULL

OpRngBitGenerator <- new_Op("OpRngBitGenerator", "rng_bit_generator")

#' @rdname hlo_rng_bit_generator
#' @export
infer_types_rng_bit_generator <- function(
  initial_state,
  rng_algorithm,
  dtype,
  shape_out
) {
  stopifnot(inherits(initial_state@type, TensorType))

  # initial_state must be 1-D ui64
  init_shape <- shape(initial_state)
  if (length(init_shape) != 1L) {
    cli_abort("initial_state must be a 1-D tensor")
  }
  if (
    !inherits(initial_state@type@dtype, UnsignedType) ||
      initial_state@type@dtype@value != 64L
  ) {
    cli_abort("initial_state must have dtype ui64")
  }

  # Validate algorithm-specific state size constraints
  algo <- as.character(rng_algorithm)
  state_size <- init_shape[[1L]]
  if (algo == "THREE_FRY") {
    if (state_size != 2L) {
      cli_abort("THREE_FRY requires size(initial_state) = 2")
    }
  } else if (algo == "PHILOX") {
    if (!(state_size %in% c(2L, 3L))) {
      cli_abort("PHILOX requires size(initial_state) to be 2 or 3")
    }
  } else if (algo != "DEFAULT") {
    cli_abort("rng_algorithm must be one of DEFAULT, THREE_FRY, PHILOX")
  }

  out_dtype <- as_dtype(dtype)
  assert_one_of(out_dtype, IntegerType, UnsignedType, FloatType)
  out_shape <- as.integer(shape_out)

  ValueTypes(list(
    # output_state: same type as initial_state
    ValueType(
      TensorType(
        dtype = UnsignedType(64L),
        shape = Shape(init_shape)
      )
    ),
    # output: arbitrary integer or float tensor
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
  shape_out
) {
  algo <- match.arg(rng_algorithm)
  hlo_rng_bit_generator_impl(
    values = list(initial_state = initial_state),
    custom_attrs = list(
      rng_algorithm = algo,
      dtype = dtype,
      shape_out = as.integer(shape_out)
    )
  )
}

method(repr, OpRngBitGenerator) <- function(x, ...) {
  paste0(
    repr(x@outputs),
    " = \"stablehlo.rng_bit_generator\" ",
    "(",
    repr(x@inputs@values),
    ") {\n",
    "rng_algorithm = #stablehlo<rng_algorithm ",
    x@inputs@custom_attrs$rng_algorithm,
    ">\n}",
    ": ",
    repr(x@signature)
  )
}
