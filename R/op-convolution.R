#' @include op.R hlo.R type_inference.R
NULL

OpConvolution <- new_Op("OpConvolution", "convolution")

PRECISION_VALUES <- c("DEFAULT", "HIGH", "HIGHEST")

#' @title ConvDimensionNumbers
#' @description
#' Represents the dimension numbers used by [`hlo_convolution`].
#' All indices are 0-based.
#'
#' For an input/output tensor of rank `N`, the batch dimension, the feature
#' dimension and the `N - 2` spatial dimensions partition `[0, N)`. Likewise
#' for the kernel, the input feature, output feature and spatial dimensions
#' partition `[0, N)`.
#' @param input_batch_dimension (`integer(1)`)\cr
#'   Batch dimension of the input.
#' @param input_feature_dimension (`integer(1)`)\cr
#'   Feature dimension of the input.
#' @param input_spatial_dimensions (`integer()`)\cr
#'   Spatial dimensions of the input.
#' @param kernel_input_feature_dimension (`integer(1)`)\cr
#'   Input-feature dimension of the kernel.
#' @param kernel_output_feature_dimension (`integer(1)`)\cr
#'   Output-feature dimension of the kernel.
#' @param kernel_spatial_dimensions (`integer()`)\cr
#'   Spatial dimensions of the kernel.
#' @param output_batch_dimension (`integer(1)`)\cr
#'   Batch dimension of the output.
#' @param output_feature_dimension (`integer(1)`)\cr
#'   Feature dimension of the output.
#' @param output_spatial_dimensions (`integer()`)\cr
#'   Spatial dimensions of the output.
#' @return (`ConvDimensionNumbers`)
#' @export
ConvDimensionNumbers <- function(
  input_batch_dimension,
  input_feature_dimension,
  input_spatial_dimensions,
  kernel_input_feature_dimension,
  kernel_output_feature_dimension,
  kernel_spatial_dimensions,
  output_batch_dimension,
  output_feature_dimension,
  output_spatial_dimensions
) {
  assert_int(input_batch_dimension)
  assert_int(input_feature_dimension)
  assert_int(kernel_input_feature_dimension)
  assert_int(kernel_output_feature_dimension)
  assert_int(output_batch_dimension)
  assert_int(output_feature_dimension)

  structure(
    list(
      input_batch_dimension = as.integer(input_batch_dimension),
      input_feature_dimension = as.integer(input_feature_dimension),
      input_spatial_dimensions = as.integer(input_spatial_dimensions),
      kernel_input_feature_dimension = as.integer(
        kernel_input_feature_dimension
      ),
      kernel_output_feature_dimension = as.integer(
        kernel_output_feature_dimension
      ),
      kernel_spatial_dimensions = as.integer(kernel_spatial_dimensions),
      output_batch_dimension = as.integer(output_batch_dimension),
      output_feature_dimension = as.integer(output_feature_dimension),
      output_spatial_dimensions = as.integer(output_spatial_dimensions)
    ),
    class = "ConvDimensionNumbers"
  )
}

#' @export
repr.ConvDimensionNumbers <- function(x, ...) {
  n_spatial <- length(x$input_spatial_dimensions)
  rank <- n_spatial + 2L

  layout <- function(rank, slots) {
    parts <- character(rank)
    for (i in seq_along(slots$indices)) {
      parts[slots$indices[i] + 1L] <- slots$labels[i]
    }
    paste0("[", paste(parts, collapse = ", "), "]")
  }

  spatial_labels <- as.character(seq_len(n_spatial) - 1L)

  input_str <- layout(
    rank,
    list(
      indices = c(
        x$input_batch_dimension,
        x$input_feature_dimension,
        x$input_spatial_dimensions
      ),
      labels = c("b", "f", spatial_labels)
    )
  )
  kernel_str <- layout(
    rank,
    list(
      indices = c(
        x$kernel_input_feature_dimension,
        x$kernel_output_feature_dimension,
        x$kernel_spatial_dimensions
      ),
      labels = c("i", "o", spatial_labels)
    )
  )
  output_str <- layout(
    rank,
    list(
      indices = c(
        x$output_batch_dimension,
        x$output_feature_dimension,
        x$output_spatial_dimensions
      ),
      labels = c("b", "f", spatial_labels)
    )
  )

  sprintf("#stablehlo.conv<%sx%s->%s>", input_str, kernel_str, output_str)
}

# Custom attribute holding the precision_config; rendered as
# `[#stablehlo<precision DEFAULT>, #stablehlo<precision DEFAULT>]`.
PrecisionConfig <- function(values) {
  structure(as.character(values), class = "PrecisionConfig")
}

#' @export
repr.PrecisionConfig <- function(x, ...) {
  parts <- vapply(
    unclass(x),
    function(v) sprintf("#stablehlo<precision %s>", v),
    character(1)
  )
  sprintf("[%s]", paste(parts, collapse = ", "))
}

#' @rdname hlo_convolution
#' @export
infer_types_convolution <- function(
  lhs,
  rhs,
  dimension_numbers,
  precision_config,
  window_strides,
  padding,
  lhs_dilation,
  rhs_dilation,
  window_reversal,
  feature_group_count,
  batch_group_count
) {
  assert_class(dimension_numbers, "ConvDimensionNumbers")
  assert_class(precision_config, "PrecisionConfig")
  assert_vts_are_tensors(lhs, rhs)
  assert_const(window_strides, dtype = IntegerType(64L), ndims = 1L)
  assert_const(padding, dtype = IntegerType(64L), ndims = 2L)
  assert_const(lhs_dilation, dtype = IntegerType(64L), ndims = 1L)
  assert_const(rhs_dilation, dtype = IntegerType(64L), ndims = 1L)
  assert_const(window_reversal, dtype = BooleanType(), ndims = 1L)
  assert_const(feature_group_count, dtype = IntegerType(64L), shape = integer())
  assert_const(batch_group_count, dtype = IntegerType(64L), shape = integer())

  lhs_shape <- shape(lhs)
  rhs_shape <- shape(rhs)
  rank_lhs <- length(lhs_shape)
  rank_rhs <- length(rhs_shape)

  # (C1)
  if (rank_lhs != rank_rhs) {
    cli_abort(c(
      "{.arg lhs} and {.arg rhs} must have the same rank.",
      x = "Got ranks {rank_lhs} and {rank_rhs}."
    ))
  }
  if (rank_lhs < 2L) {
    cli_abort(c(
      "{.arg lhs} and {.arg rhs} must have rank >= 2.",
      x = "Got rank {rank_lhs}."
    ))
  }
  rank <- rank_lhs
  n_spatial <- rank - 2L

  strides <- as.integer(window_strides$data)
  pad <- padding$data
  storage.mode(pad) <- "integer"
  lhs_dil <- as.integer(lhs_dilation$data)
  rhs_dil <- as.integer(rhs_dilation$data)
  reversal <- as.logical(window_reversal$data)
  fg_count <- as.integer(feature_group_count$data)
  bg_count <- as.integer(batch_group_count$data)

  ibd <- dimension_numbers$input_batch_dimension
  ifd <- dimension_numbers$input_feature_dimension
  isd <- dimension_numbers$input_spatial_dimensions
  kifd <- dimension_numbers$kernel_input_feature_dimension
  kofd <- dimension_numbers$kernel_output_feature_dimension
  ksd <- dimension_numbers$kernel_spatial_dimensions
  obd <- dimension_numbers$output_batch_dimension
  ofd <- dimension_numbers$output_feature_dimension
  osd <- dimension_numbers$output_spatial_dimensions

  # (C2)
  if (length(strides) != n_spatial) {
    cli_abort(c(
      "{.arg window_strides} must have length N - 2.",
      x = "Expected length {n_spatial}, got {length(strides)}."
    ))
  }
  # (C3)
  if (any(strides <= 0L)) {
    cli_abort(c(
      "{.arg window_strides} must be positive.",
      x = "Got {vec_repr(strides)}."
    ))
  }
  # (C4)
  if (nrow(pad) != n_spatial || ncol(pad) != 2L) {
    cli_abort(c(
      "{.arg padding} must have shape [N - 2, 2].",
      x = "Expected shape [{n_spatial}, 2], got [{nrow(pad)}, {ncol(pad)}]."
    ))
  }
  # (C5)
  if (length(lhs_dil) != n_spatial) {
    cli_abort(c(
      "{.arg lhs_dilation} must have length N - 2.",
      x = "Expected length {n_spatial}, got {length(lhs_dil)}."
    ))
  }
  # (C6)
  if (any(lhs_dil <= 0L)) {
    cli_abort(c(
      "{.arg lhs_dilation} must be positive.",
      x = "Got {vec_repr(lhs_dil)}."
    ))
  }
  # (C7)
  if (length(rhs_dil) != n_spatial) {
    cli_abort(c(
      "{.arg rhs_dilation} must have length N - 2.",
      x = "Expected length {n_spatial}, got {length(rhs_dil)}."
    ))
  }
  # (C8)
  if (any(rhs_dil <= 0L)) {
    cli_abort(c(
      "{.arg rhs_dilation} must be positive.",
      x = "Got {vec_repr(rhs_dil)}."
    ))
  }
  # (C9)
  if (length(reversal) != n_spatial) {
    cli_abort(c(
      "{.arg window_reversal} must have length N - 2.",
      x = "Expected length {n_spatial}, got {length(reversal)}."
    ))
  }

  # (C21)
  if (fg_count <= 0L) {
    cli_abort(c(
      "{.arg feature_group_count} must be positive.",
      x = "Got {fg_count}."
    ))
  }
  # (C22)
  if (bg_count <= 0L) {
    cli_abort(c(
      "{.arg batch_group_count} must be positive.",
      x = "Got {bg_count}."
    ))
  }
  # (C23)
  if (fg_count != 1L && bg_count != 1L) {
    cli_abort(c(
      "At least one of {.arg feature_group_count} or {.arg batch_group_count} must be 1.",
      x = "Got feature_group_count = {fg_count} and batch_group_count = {bg_count}."
    ))
  }

  # (C12)
  if (length(isd) != n_spatial) {
    cli_abort(c(
      "{.arg input_spatial_dimensions} must have length N - 2.",
      x = "Expected length {n_spatial}, got {length(isd)}."
    ))
  }
  # (C17)
  if (length(ksd) != n_spatial) {
    cli_abort(c(
      "{.arg kernel_spatial_dimensions} must have length N - 2.",
      x = "Expected length {n_spatial}, got {length(ksd)}."
    ))
  }
  # (C19)
  if (length(osd) != n_spatial) {
    cli_abort(c(
      "{.arg output_spatial_dimensions} must have length N - 2.",
      x = "Expected length {n_spatial}, got {length(osd)}."
    ))
  }

  # (C13)
  input_dimensions <- c(ibd, isd, ifd)
  if (anyDuplicated(input_dimensions)) {
    error_dimension_uniqueness(
      arg = "input_batch_dimension/input_spatial_dimensions/input_feature_dimension",
      dimensions = input_dimensions
    )
  }
  if (any(input_dimensions < 0L) || any(input_dimensions >= rank)) {
    error_index_out_of_bounds(
      arg = "input_batch_dimension/input_spatial_dimensions/input_feature_dimension",
      index = input_dimensions,
      lower = 0L,
      upper = rank
    )
  }

  # (C18)
  kernel_dimensions <- c(ksd, kifd, kofd)
  if (anyDuplicated(kernel_dimensions)) {
    error_dimension_uniqueness(
      arg = "kernel_spatial_dimensions/kernel_input_feature_dimension/kernel_output_feature_dimension",
      dimensions = kernel_dimensions
    )
  }
  if (any(kernel_dimensions < 0L) || any(kernel_dimensions >= rank)) {
    error_index_out_of_bounds(
      arg = "kernel_spatial_dimensions/kernel_input_feature_dimension/kernel_output_feature_dimension",
      index = kernel_dimensions,
      lower = 0L,
      upper = rank
    )
  }

  # (C20)
  output_dimensions <- c(obd, osd, ofd)
  if (anyDuplicated(output_dimensions)) {
    error_dimension_uniqueness(
      arg = "output_batch_dimension/output_spatial_dimensions/output_feature_dimension",
      dimensions = output_dimensions
    )
  }
  if (any(output_dimensions < 0L) || any(output_dimensions >= rank)) {
    error_index_out_of_bounds(
      arg = "output_batch_dimension/output_spatial_dimensions/output_feature_dimension",
      index = output_dimensions,
      lower = 0L,
      upper = rank
    )
  }

  input_batch_size <- lhs_shape[ibd + 1L]
  input_feature_size <- lhs_shape[ifd + 1L]
  kernel_input_feature_size <- rhs_shape[kifd + 1L]
  kernel_output_feature_size <- rhs_shape[kofd + 1L]

  # (C10)
  if (input_batch_size %% bg_count != 0L) {
    cli_abort(c(
      "dim(lhs, input_batch_dimension) must be divisible by {.arg batch_group_count}.",
      x = "Got dim = {input_batch_size}, batch_group_count = {bg_count}."
    ))
  }
  # (C11)
  if (input_feature_size %% fg_count != 0L) {
    cli_abort(c(
      "dim(lhs, input_feature_dimension) must be divisible by {.arg feature_group_count}.",
      x = "Got dim = {input_feature_size}, feature_group_count = {fg_count}."
    ))
  }
  # (C14)
  if (kernel_input_feature_size != input_feature_size %/% fg_count) {
    cli_abort(c(
      "dim(rhs, kernel_input_feature_dimension) must equal dim(lhs, input_feature_dimension) / feature_group_count.",
      x = "Got dim = {kernel_input_feature_size}, expected {input_feature_size %/% fg_count}."
    ))
  }
  # (C15)
  if (kernel_output_feature_size %% bg_count != 0L) {
    cli_abort(c(
      "dim(rhs, kernel_output_feature_dimension) must be divisible by {.arg batch_group_count}.",
      x = "Got dim = {kernel_output_feature_size}, batch_group_count = {bg_count}."
    ))
  }
  # (C16)
  if (kernel_output_feature_size %% fg_count != 0L) {
    cli_abort(c(
      "dim(rhs, kernel_output_feature_dimension) must be divisible by {.arg feature_group_count}.",
      x = "Got dim = {kernel_output_feature_size}, feature_group_count = {fg_count}."
    ))
  }

  # (C24)
  if (length(precision_config) != 2L) {
    cli_abort(c(
      "{.arg precision_config} must have length 2.",
      x = "Got length {length(precision_config)}."
    ))
  }
  if (!all(unclass(precision_config) %in% PRECISION_VALUES)) {
    cli_abort(c(
      "{.arg precision_config} entries must be one of {.val {PRECISION_VALUES}}.",
      x = "Got {.val {unclass(precision_config)}}."
    ))
  }

  # (C27)
  if (lhs$type$dtype != rhs$type$dtype) {
    cli_abort(c(
      "{.arg lhs} and {.arg rhs} must have the same element type.",
      x = "Got {.val {lhs$type$dtype}} and {.val {rhs$type$dtype}}."
    ))
  }

  # (C25, C26): compute result shape
  result_shape <- integer(rank)
  result_shape[obd + 1L] <- input_batch_size %/% bg_count
  result_shape[ofd + 1L] <- kernel_output_feature_size

  for (sd in seq_len(n_spatial)) {
    lhs_dim <- isd[sd]
    rhs_dim <- ksd[sd]
    lhs_size <- lhs_shape[lhs_dim + 1L]
    rhs_size <- rhs_shape[rhs_dim + 1L]

    dilated_input <- if (lhs_size == 0L) {
      0L
    } else {
      (lhs_size - 1L) * lhs_dil[sd] + 1L
    }
    padded_input <- pad[sd, 1L] + dilated_input + pad[sd, 2L]
    dilated_window <- if (rhs_size == 0L) {
      0L
    } else {
      (rhs_size - 1L) * rhs_dil[sd] + 1L
    }
    num_windows <- if (padded_input == 0L || dilated_window > padded_input) {
      0L
    } else {
      as.integer(floor((padded_input - dilated_window) / strides[sd]) + 1L)
    }
    result_shape[osd[sd] + 1L] <- num_windows
  }

  ValueTypes(list(
    ValueType(
      TensorType(dtype = lhs$type$dtype, shape = Shape(result_shape))
    )
  ))
}

hlo_convolution_impl <- hlo_fn(OpConvolution, infer_types_convolution)

#' @title Convolution Operator
#' @description
#' Computes dot products between windows of `lhs` and slices of `rhs`.
#' See \url{https://openxla.org/stablehlo/spec#convolution} for details.
#'
#' All dimension indices are 0-based.
#' @param lhs ([`FuncValue`])\cr
#'   The input tensor (typically `[batch, spatial..., feature]`).
#' @param rhs ([`FuncValue`])\cr
#'   The kernel tensor.
#' @param dimension_numbers ([`ConvDimensionNumbers`])\cr
#'   The convolution dimension numbers describing the layout of `lhs`, `rhs`,
#'   and the result.
#' @param window_strides (`integer()`)\cr
#'   Stride of the kernel window in each spatial dimension.
#'   Length `N - 2` where `N = rank(lhs)`.
#' @param padding (`matrix`)\cr
#'   `[N - 2, 2]` integer matrix of `(low, high)` padding for each spatial
#'   dimension.
#' @param lhs_dilation (`integer()`)\cr
#'   Dilation factor applied to `lhs` (a.k.a. transpose-conv stride).
#'   Length `N - 2`. Defaults to all 1's.
#' @param rhs_dilation (`integer()`)\cr
#'   Dilation factor applied to `rhs` (a.k.a. atrous-conv rate).
#'   Length `N - 2`. Defaults to all 1's.
#' @param window_reversal (`logical()`)\cr
#'   Whether to reverse the kernel along each spatial dimension.
#'   Length `N - 2`. Defaults to all `FALSE`.
#' @param feature_group_count (`integer(1)`)\cr
#'   Number of feature groups (for grouped / depthwise convolution).
#'   Defaults to 1.
#' @param batch_group_count (`integer(1)`)\cr
#'   Number of batch groups. Defaults to 1.
#' @param precision_config (`character(2)`)\cr
#'   Two precision specifiers (one for each operand), each one of
#'   `"DEFAULT"`, `"HIGH"` or `"HIGHEST"`. Defaults to `c("DEFAULT", "DEFAULT")`.
#' @return [`FuncValue`]
#' @export
hlo_convolution <- function(
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
  precision_config = c("DEFAULT", "DEFAULT")
) {
  assert_class(dimension_numbers, "ConvDimensionNumbers")

  n_spatial <- length(dimension_numbers$input_spatial_dimensions)
  lhs_dilation <- lhs_dilation %??% rep.int(1L, n_spatial)
  rhs_dilation <- rhs_dilation %??% rep.int(1L, n_spatial)
  window_reversal <- window_reversal %??% rep.int(FALSE, n_spatial)

  one_d_int <- function(name, value) {
    value <- as.integer(value)
    constant_attr(name, value, dtype = "i64", shape = length(value))
  }

  attrs <- list(
    one_d_int("window_strides", window_strides),
    constant_attr(
      "padding",
      `storage.mode<-`(padding, "integer"),
      dtype = "i64",
      shape = dim(padding),
      simplify_dense = FALSE
    ),
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
      dtype = IntegerType(64L)
    ),
    ScalarAttr(
      name = "batch_group_count",
      value = as.integer(batch_group_count),
      dtype = IntegerType(64L)
    )
  )

  hlo_convolution_impl(
    values = list(lhs = lhs, rhs = rhs),
    custom_attrs = list(
      dimension_numbers = dimension_numbers,
      precision_config = PrecisionConfig(precision_config)
    ),
    attrs = attrs
  )
}

#' @export
repr.OpConvolution <- function(x, toplevel = TRUE, simplify_dense = TRUE, ...) {
  values_repr <- repr(x$inputs$values)
  attrs_parts <- vapply(
    x$inputs$attrs,
    function(a) repr(a, simplify_dense = simplify_dense),
    character(1)
  )

  dim_num <- x$inputs$custom_attrs$dimension_numbers
  precision <- x$inputs$custom_attrs$precision_config

  all_parts <- c(
    attrs_parts,
    paste0("dimension_numbers = ", repr(dim_num)),
    paste0("precision_config = ", repr(precision))
  )

  paste0(
    repr(x$outputs),
    " = ",
    repr(x$name),
    "(",
    values_repr,
    ") {\n",
    paste(all_parts, collapse = ",\n"),
    "\n}: ",
    repr(x$signature)
  )
}
