#' @include op.R hlo.R op-convolution.R
NULL

# `dimension_numbers` and `precision_config` are custom attributes, which the
# default renderer does not emit -- so this op needs a renderer of its own,
# exactly as `convolution` does.
render_dynamic_conv <- function(ctx) {
  attrs_parts <- vapply(
    ctx$attrs,
    function(a) repr(a, simplify_dense = TRUE),
    character(1)
  )
  all_parts <- c(
    attrs_parts,
    paste0("dimension_numbers = ", repr(ctx$custom_attrs$dimension_numbers)),
    paste0("precision_config = ", repr(ctx$custom_attrs$precision_config))
  )
  paste0(
    ctx$outputs_str,
    " = \"stablehlo.dynamic_conv\"(",
    ctx$values_str,
    ") {\n",
    paste(all_parts, collapse = ",\n"),
    "\n}: ",
    ctx$sig_str
  )
}

OpDynamicConv <- new_Op(
  "OpDynamicConv",
  "dynamic_conv",
  render = render_dynamic_conv
)

#' @rdname hlo_dynamic_conv
#' @export
infer_types_dynamic_conv <- function(
  lhs,
  rhs,
  dimension_numbers,
  padding,
  precision_config,
  window_strides,
  lhs_dilation,
  rhs_dilation,
  window_reversal,
  feature_group_count,
  batch_group_count
) {
  assert_class(dimension_numbers, "ConvDimensionNumbers")
  assert_vt_is_tensor(padding)
  n_spatial <- length(dimension_numbers$input_spatial_dimensions)

  # (C4) `padding` is `(n_spatial, 2)`.
  declared <- shape(padding)
  if (length(declared) != 2L) {
    cli_abort(c(
      "{.arg padding} must be a rank-2 tensor.",
      x = "Got shape {shapevec_repr(declared)}."
    ))
  }
  if (any(must_ne(declared, c(n_spatial, 2L)))) {
    cli_abort(c(
      "{.arg padding} must have shape ({n_spatial}, 2), one row per spatial axis.",
      x = "Got shape {shapevec_repr(declared)}."
    ))
  }

  # Everything else is `convolution`'s, and the only difference is where the
  # padding comes from -- so the static inference runs with it marked unknown.
  # Every check that does not depend on the padding still fires; the result's
  # spatial extents come back as `?`, since the padding determines them.
  infer_types_convolution(
    lhs = lhs,
    rhs = rhs,
    dimension_numbers = dimension_numbers,
    precision_config = precision_config,
    window_strides = window_strides,
    padding = r_to_constant(
      rep(NA_integer_, n_spatial * 2L),
      dtype = "i64",
      shape = c(n_spatial, 2L)
    ),
    lhs_dilation = lhs_dilation,
    rhs_dilation = rhs_dilation,
    window_reversal = window_reversal,
    feature_group_count = feature_group_count,
    batch_group_count = batch_group_count
  )
}

hlo_dynamic_conv_impl <- hlo_fn(OpDynamicConv, infer_types_dynamic_conv)

#' @templateVar mnemonic dynamic_conv
#' @templateVar not_func_variables dimension_numbers,window_strides,lhs_dilation,rhs_dilation,window_reversal,feature_group_count,batch_group_count,precision_config
#' @template op
#' @param dimension_numbers ([`ConvDimensionNumbers`])\cr
#'   Which axes of `lhs`, `rhs` and the result play which role.
#' @param window_strides,lhs_dilation,rhs_dilation,window_reversal (`integer()` | `logical()`)\cr
#'   One entry per spatial axis, as for [`hlo_convolution()`].
#' @param feature_group_count,batch_group_count (`integer(1)`)\cr
#'   Grouping, as for [`hlo_convolution()`].
#' @param precision_config (`character()`)\cr
#'   Two of `"DEFAULT"`, `"HIGH"`, `"HIGHEST"`.
#' @export
hlo_dynamic_conv <- function(
  lhs,
  rhs,
  dimension_numbers,
  window_strides,
  padding,
  lhs_dilation = NULL,
  rhs_dilation = NULL,
  window_reversal = NULL,
  feature_group_count = 1L,
  batch_group_count = 1L,
  precision_config = c("DEFAULT", "DEFAULT"),
  output_types = NULL
) {
  assert_class(dimension_numbers, "ConvDimensionNumbers")
  n_spatial <- length(dimension_numbers$input_spatial_dimensions)
  # As `hlo_convolution()` does: the custom attribute must be a
  # `PrecisionConfig`, since that is what `repr()` renders.
  precision_config <- PrecisionConfig(
    normalize_precision_config(precision_config)
  )
  one_d_int <- function(name, value) {
    value <- as.integer(value)
    constant_attr(name, value, dtype = "i64", shape = length(value))
  }
  lhs_dilation <- lhs_dilation %??% rep.int(1L, n_spatial)
  rhs_dilation <- rhs_dilation %??% rep.int(1L, n_spatial)
  window_reversal <- window_reversal %??% rep.int(FALSE, n_spatial)

  hlo_dynamic_conv_impl(
    values = list(lhs = lhs, rhs = rhs, padding = padding),
    output_types = output_types,
    attrs = list(
      one_d_int("window_strides", window_strides),
      one_d_int("lhs_dilation", lhs_dilation),
      one_d_int("rhs_dilation", rhs_dilation),
      constant_attr(
        "window_reversal",
        as.logical(window_reversal),
        dtype = "i1",
        shape = length(window_reversal)
      ),
      ScalarAttr(
        name = "feature_group_count",
        value = as.integer(feature_group_count),
        dtype = as_dtype("i64")
      ),
      ScalarAttr(
        name = "batch_group_count",
        value = as.integer(batch_group_count),
        dtype = as_dtype("i64")
      )
    ),
    custom_attrs = list(
      dimension_numbers = dimension_numbers,
      precision_config = precision_config
    )
  )
}
