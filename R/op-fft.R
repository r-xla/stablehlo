#' @include op.R hlo.R
NULL

OpFft <- new_Op("OpFft", "fft")

# Map fft_type to (operand_kind, result_kind) where each is "real" or "complex"
# Used by both inference and the public function for clarity.
.fft_type_kinds <- list(
  FFT = list(operand = "complex", result = "complex"),
  IFFT = list(operand = "complex", result = "complex"),
  RFFT = list(operand = "real", result = "complex"),
  IRFFT = list(operand = "complex", result = "real")
)

.fft_real_to_complex <- function(real_dtype) {
  ComplexType(real_dtype$value)
}

.fft_complex_to_real <- function(complex_dtype) {
  FloatType(complex_dtype$value)
}

#' @rdname hlo_fft
#' @export
infer_types_fft <- function(operand, fft_type, fft_length) {
  assert_vt_is_tensor(operand)

  if (!test_choice(fft_type, names(.fft_type_kinds))) {
    cli_abort(c(
      "{.arg fft_type} must be one of {.val {names(.fft_type_kinds)}}.",
      x = "Got {.val {fft_type}}."
    ))
  }
  kinds <- .fft_type_kinds[[fft_type]]

  assert_const(fft_length, dtype = IntegerType(64L), ndims = 1L)
  fft_len <- as.integer(fft_length$data)
  L <- length(fft_len)

  # (C3) 1 <= size(fft_length) <= 3.
  if (L < 1L || L > 3L) {
    cli_abort(c(
      "{.arg fft_length} must have 1 to 3 elements.",
      x = "Got {.val {L}}."
    ))
  }

  operand_dtype <- operand$type$dtype
  operand_shape <- shape(operand)
  rank <- length(operand_shape)

  # (C2) operand element type constraint.
  if (kinds$operand == "real") {
    if (!test_class(operand_dtype, "FloatType")) {
      cli_abort(c(
        "{.arg operand} must have a floating-point dtype for {.val {fft_type}}.",
        x = "Got {.val {operand_dtype}}."
      ))
    }
    result_dtype <- .fft_real_to_complex(operand_dtype)
  } else {
    if (!test_class(operand_dtype, "ComplexType")) {
      cli_abort(c(
        "{.arg operand} must have a complex dtype for {.val {fft_type}}.",
        x = "Got {.val {operand_dtype}}."
      ))
    }
    result_dtype <- if (kinds$result == "complex") {
      operand_dtype
    } else {
      .fft_complex_to_real(operand_dtype)
    }
  }

  # (C1) size(fft_length) <= rank(operand).
  if (L > rank) {
    cli_abort(c(
      "{.arg fft_length} length must not exceed rank of {.arg operand}.",
      x = "Got fft_length of length {L} and operand of rank {rank}."
    ))
  }

  fft_axes <- seq.int(rank - L + 1L, rank)

  # (C4) For the tensor of floating-point type among operand/result,
  # shape(real)[-L:] must equal fft_length.
  if (kinds$operand == "real") {
    real_tail <- operand_shape[fft_axes]
    if (!identical(real_tail, fft_len)) {
      cli_abort(c(
        "{.arg operand} trailing {L} dimension{?s} must equal {.arg fft_length}.",
        x = "Got operand shape {shapevec_repr(operand_shape)} and fft_length = {vec_repr(fft_len)}."
      ))
    }
  }

  # (C5) shape(result) = shape(operand) except for special cases.
  result_shape <- operand_shape
  if (fft_type == "RFFT") {
    last <- operand_shape[[rank]]
    result_shape[[rank]] <- if (last == 0L) 0L else as.integer(last %/% 2L + 1L)
  } else if (fft_type == "IRFFT") {
    # (C4) shape(result)[-L:] must equal fft_length except for the last dim,
    # where dim(operand, -1) = fft_length[L] / 2 + 1.
    expected_last_operand <- if (fft_len[[L]] == 0L) {
      0L
    } else {
      as.integer(fft_len[[L]] %/% 2L + 1L)
    }
    operand_tail_lead <- operand_shape[fft_axes[seq_len(L - 1L)]]
    fft_lead <- fft_len[seq_len(L - 1L)]
    if (
      !identical(operand_tail_lead, fft_lead) ||
        operand_shape[[rank]] != expected_last_operand
    ) {
      cli_abort(c(
        "{.arg operand} trailing dimensions must match {.arg fft_length}.",
        x = paste0(
          "Got operand shape {shapevec_repr(operand_shape)} and fft_length = ",
          "{vec_repr(fft_len)}; ",
          "expected last operand dim {.val {expected_last_operand}} ",
          "(= fft_length[{L}] / 2 + 1)."
        )
      ))
    }
    result_shape[fft_axes] <- fft_len
  }

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = result_dtype,
        shape = Shape(result_shape)
      )
    )
  ))
}

hlo_fft_impl <- hlo_fn(OpFft, infer_types_fft)

#' @templateVar mnemonic fft
#' @template op
#' @param operand (`FuncValue`)\cr
#'   Tensor of floating-point or complex type.
#' @param fft_type (`character(1)`)\cr
#'   One of `"FFT"`, `"IFFT"`, `"RFFT"`, `"IRFFT"`.
#' @param fft_length (`integer()`)\cr
#'   Length 1, 2, or 3 vector of `i64` values giving the FFT lengths along
#'   the trailing dimensions.
#' @export
hlo_fft <- function(
  operand,
  fft_type = c("FFT", "IFFT", "RFFT", "IRFFT"),
  fft_length
) {
  fft_type <- match.arg(fft_type)
  fft_length <- as.integer(fft_length)

  hlo_fft_impl(
    values = list(operand = operand),
    attrs = list(
      constant_attr(
        "fft_length",
        fft_length,
        dtype = "i64",
        shape = length(fft_length)
      )
    ),
    custom_attrs = list(fft_type = fft_type)
  )
}

#' @export
repr.OpFft <- function(x, simplify_dense = TRUE, ...) {
  attrs_repr <- vapply(
    x$inputs$attrs,
    function(a) repr(a, simplify_dense = simplify_dense),
    character(1)
  )
  fft_type_repr <- paste0(
    "fft_type = #stablehlo<fft_type ",
    x$inputs$custom_attrs$fft_type,
    ">"
  )
  all_attrs <- paste(c(fft_type_repr, attrs_repr), collapse = ",\n")

  paste0(
    repr(x$outputs),
    " = ",
    repr(x$name),
    " (",
    repr(x$inputs$values),
    ") {\n",
    all_attrs,
    "\n}: ",
    repr(x$signature)
  )
}
