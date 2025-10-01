#' @include op.R hlo.R type_inference.R
NULL

# RNG Distribution Enum
RngDistribution <- new_enum(
  "RngDistribution",
  c("UNIFORM", "NORMAL")
)

OpRng <- new_Op("OpRng", "rng")

# Type inference for RNG operation
infer_types_rng <- function(a, b, shape, rng_distribution) {
  stopifnot(inherits(a@type, TensorType))
  stopifnot(inherits(b@type, TensorType))
  stopifnot(inherits(shape@type, TensorType))

  # Check that a and b have the same element type
  if (a@type@dtype != b@type@dtype) {
    stop("Element types of 'a' and 'b' must be the same")
  }

  # Check that a and b are 0-dimensional tensors (scalars)
  if (length(a@type@shape@dims) != 0L) {
    stop("'a' must be a 0-dimensional tensor (scalar)")
  }
  if (length(b@type@shape@dims) != 0L) {
    stop("'b' must be a 0-dimensional tensor (scalar)")
  }

  # Check that shape is a 1-dimensional tensor of type si64
  if (length(shape@type@shape@dims) != 1L) {
    stop("'shape' must be a 1-dimensional tensor")
  }
  if (!inherits(shape@type@dtype, IntegerType) || shape@type@dtype@value != "i64") {
    stop("'shape' must be of type si64")
  }

  # For NORMAL distribution, a and b must be floating-point types
  if (rng_distribution@value == "NORMAL") {
    if (!inherits(a@type@dtype, FloatType)) {
      stop("For NORMAL distribution, 'a' and 'b' must be floating-point types")
    }
  }

  # Create output type with same element type as a and b, but with the specified shape
  # Note: We need to get the shape from the shape constant
  # For now, we'll create a placeholder - the actual shape will be resolved during execution
  output_type <- ValueType(TensorType(dtype = a@type@dtype, shape = Shape(integer())))

  ValueTypes(list(output_type))
}

hlo_rng_impl <- hlo_fn(OpRng, infer_types_rng)

#' @title Create RNG Operation
#' @description
#' Generates random numbers using the specified distribution algorithm.
#'
#' If `rng_distribution = "UNIFORM"`, then the random numbers are generated
#' following the uniform distribution over the interval `[a, b)`. If `a >= b`,
#' the behavior is undefined.
#'
#' If `rng_distribution = "NORMAL"`, then the random numbers are generated
#' following the normal distribution with mean = `a` and standard deviation = `b`.
#' If `b < 0`, the behavior is undefined.
#'
#' @param a ([`FuncVariable`])\cr
#'   A scalar tensor of integer, boolean, or floating-point type.
#'   For UNIFORM: lower bound of the interval.
#'   For NORMAL: mean of the distribution.
#' @param b ([`FuncVariable`])\cr
#'   A scalar tensor of integer, boolean, or floating-point type.
#'   For UNIFORM: upper bound of the interval.
#'   For NORMAL: standard deviation of the distribution.
#' @param shape ([`FuncVariable`])\cr
#'   A 1-dimensional tensor constant of type si64 specifying the shape of the output.
#' @param rng_distribution (`character(1)`)\cr
#'   One of: "UNIFORM" or "NORMAL".
#' @return ([`FuncVariable`])\cr
#'   A tensor of the same element type as `a` and `b` with the specified shape.
#' @export
#' @examples
#' func <- local_func()
#' a <- hlo_scalar(0.0, dtype = "f32")
#' b <- hlo_scalar(1.0, dtype = "f32")
#' shape <- hlo_tensor(c(3L, 3L), dtype = "i64", shape = 2L)
#' result <- hlo_rng(a, b, shape, "UNIFORM")
hlo_rng <- function(a, b, shape, rng_distribution = "UNIFORM") {
  if (!(rng_distribution %in% c("UNIFORM", "NORMAL"))) {
    stop("rng_distribution must be either 'UNIFORM' or 'NORMAL'")
  }

  rng_dist_enum <- RngDistribution(rng_distribution)

  hlo_rng_impl(
    values = list(a = a, b = b, shape = shape),
    custom_attrs = list(rng_distribution = rng_dist_enum)
  )
}

method(repr, OpRng) <- function(x, toplevel = TRUE, simplify_dense = TRUE) {
  paste0(
    repr(x@outputs),
    " = stablehlo.rng ",
    repr(x@inputs@values),
    " { rng_distribution = #stablehlo<rng_distribution ",
    x@inputs@custom_attrs$rng_distribution@value,
    "> } : ",
    repr(x@signature)
  )
}
