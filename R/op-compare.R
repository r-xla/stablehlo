#' @include op.R hlo.R
NULL

render_compare <- function(ctx) {
  paste0(
    ctx$outputs_str,
    " = stablehlo.compare ",
    ctx$custom_attrs$comparison_direction,
    ", ",
    ctx$values_str,
    ", ",
    ctx$custom_attrs$compare_type,
    " : ",
    ctx$sig_str
  )
}

OpCompare <- new_Op("OpCompare", "compare", render = render_compare)

#' @rdname hlo_compare
#' @export
infer_types_compare <- function(
  lhs,
  rhs,
  comparison_direction,
  compare_type
) {
  assert_vts_are_tensors(lhs, rhs)

  # (I3)
  cds <- c("EQ", "NE", "GE", "GT", "LE", "LT")
  if (!(comparison_direction %in% cds)) {
    cli_abort(c(
      "{.arg comparison_direction} must be one of {.val {cds}}.",
      x = "Got {.val {comparison_direction}}."
    ))
  }

  # (I4)
  cts <- c("FLOAT", "TOTALORDER", "SIGNED", "UNSIGNED")
  if (!(compare_type %in% cts)) {
    cli_abort(c(
      "{.arg compare_type} must be one of {.val {cts}}.",
      x = "Got {.val {compare_type}}."
    ))
  }

  # (C1), (C2)
  assert_vt_equal(lhs, rhs)

  # (C3)
  dtype <- lhs$type$dtype
  if (inherits(dtype, "IntegerType")) {
    if (compare_type != "SIGNED") {
      cli_abort(
        "{.arg compare_type} must be SIGNED for signed integer data types."
      )
    }
  } else if (
    inherits(dtype, "UIntegerType") || inherits(dtype, "BooleanType")
  ) {
    if (compare_type != "UNSIGNED") {
      cli_abort(
        "{.arg compare_type} must be UNSIGNED for unsigned integer or boolean data types."
      )
    }
  } else if (inherits(dtype, "FloatType")) {
    if (!(compare_type %in% c("FLOAT", "TOTALORDER"))) {
      cli_abort(
        "{.arg compare_type} must be FLOAT or TOTALORDER for floating-point data types."
      )
    }
  }

  ValueTypes(list(
    ValueType(
      TensorType(
        dtype = BooleanType(),
        shape = Shape(shape(lhs))
      )
    )
  ))
}

hlo_compare_impl <- hlo_fn(
  OpCompare,
  infer_types_compare
)

#' @templateVar mnemonic compare
#' @template op
#' @export
hlo_compare <- function(
  lhs,
  rhs,
  comparison_direction,
  compare_type,
  output_types = NULL
) {
  hlo_compare_impl(
    values = list(lhs = lhs, rhs = rhs),
    output_types = output_types,
    custom_attrs = list(
      comparison_direction = comparison_direction,
      compare_type = compare_type
    )
  )
}
