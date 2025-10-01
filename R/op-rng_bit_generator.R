#' @include op.R hlo.R type_inference.R
NULL

# RNG Algorithm Enum
RngAlgorithm <- new_enum(
  "RngAlgorithm",
  c("DEFAULT", "THREE_FRY", "PHILOX")
)

OpRngBitGenerator <- new_Op("OpRngBitGenerator", "rng_bit_generator")

# Type inference for RNG bit generator operation
infer_types_rng_bit_generator <- function(rng_algorithm, initial_state) {
  stopifnot(inherits(initial_state@type, TensorType))

  # Check that initial_state is a 1-dimensional tensor of type ui64
  if (length(initial_state@type@shape@dims) != 1L) {
    stop("'initial_state' must be a 1-dimensional tensor")
  }
  if (!inherits(initial_state@type@dtype, IntegerType) ||
      initial_state@type@dtype@value != "ui64") {
    stop("'initial_state' must be of type ui64")
  }

  # Check size constraints based on algorithm
  state_size <- initial_state@type@shape@dims[1L]
  if (rng_algorithm@value == "THREE_FRY" && state_size != 2L) {
    stop("For THREE_FRY algorithm, initial_state must have size 2")
  }
  if (rng_algorithm@value == "PHILOX" && !(state_size %in% c(2L, 3L))) {
    stop("For PHILOX algorithm, initial_state must have size 2 or 3")
  }
  # DEFAULT has implementation-defined size, so no check needed

  # The output has two components:
  # 1. output_state: same type as initial_state
  # 2. output: tensor of integer or floating-point type
  # For now, we'll create a placeholder output type - the actual type will be resolved during execution
  output_state_type <- initial_state
  output_type <- ValueType(TensorType(dtype = IntegerType("ui64"), shape = Shape(integer())))

  ValueTypes(list(output_state_type, output_type))
}

hlo_rng_bit_generator_impl <- hlo_fn(OpRngBitGenerator, infer_types_rng_bit_generator)

#' @title Create RNG Bit Generator Operation
#' @description
#' Returns an output filled with uniform random bits and an updated output state
#' using the pseudorandom number generator algorithm given an initial state.
#'
#' The output is guaranteed to be a deterministic function of `initial_state`,
#' but it is not guaranteed to be deterministic between implementations.
#'
#' @param rng_algorithm (`character(1)`)\cr
#'   One of: "DEFAULT", "THREE_FRY", or "PHILOX".
#'   - DEFAULT: Implementation-defined algorithm.
#'   - THREE_FRY: Implementation-defined variant of the Threefry algorithm.
#'   - PHILOX: Implementation-defined variant of the Philox algorithm.
#' @param initial_state ([`FuncVariable`])\cr
#'   A 1-dimensional tensor of type ui64 containing the initial state.
#'   Size constraints:
#'   - DEFAULT: implementation-defined size
#'   - THREE_FRY: size must be 2
#'   - PHILOX: size must be 2 or 3
#' @return (`list()` of [`FuncVariable`])\cr
#'   A list containing two elements:
#'   - output_state: Updated state tensor of the same type as initial_state
#'   - output: Tensor of integer or floating-point type filled with random bits
#' @export
#' @examples
#' func <- local_func()
#' initial_state <- hlo_tensor(c(1L, 2L), dtype = "ui64", shape = 2L)
#' result <- hlo_rng_bit_generator("THREE_FRY", initial_state)
#' output_state <- result[[1]]
#' output <- result[[2]]
hlo_rng_bit_generator <- function(rng_algorithm = "DEFAULT", initial_state) {
  if (!(rng_algorithm %in% c("DEFAULT", "THREE_FRY", "PHILOX"))) {
    stop("rng_algorithm must be one of: 'DEFAULT', 'THREE_FRY', 'PHILOX'")
  }

  rng_algo_enum <- RngAlgorithm(rng_algorithm)

  hlo_rng_bit_generator_impl(
    values = list(initial_state = initial_state),
    custom_attrs = list(rng_algorithm = rng_algo_enum)
  )
}

method(repr, OpRngBitGenerator) <- function(x, toplevel = TRUE, simplify_dense = TRUE) {
  paste0(
    repr(x@outputs),
    " = stablehlo.rng_bit_generator ",
    repr(x@inputs@values),
    " { rng_algorithm = #stablehlo<rng_algorithm ",
    x@inputs@custom_attrs$rng_algorithm@value,
    "> } : ",
    repr(x@signature)
  )
}
