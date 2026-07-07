render_dot_general <- function(ctx) {
  precision_config <- ctx$custom_attrs$precision_config
  precision_repr <- if (is.null(precision_config)) {
    ""
  } else {
    paste0(", precision = [", paste0(precision_config, collapse = ", "), "]")
  }
  ddn <- ctx$custom_attrs$dot_dimension_numbers
  batching_repr <- if (is.null(ddn$batching_dims)) {
    ""
  } else {
    paste0(
      "batching_dims = [",
      paste0(ddn$batching_dims[[1L]], collapse = ", "),
      "] x [",
      paste0(ddn$batching_dims[[2L]], collapse = ", "),
      "], "
    )
  }
  paste0(
    ctx$outputs_str,
    " = ",
    "stablehlo.dot_general ",
    ctx$values_str,
    ", ",
    batching_repr,
    "contracting_dims = [",
    paste0(ddn$contracting_dims[[1L]], collapse = ", "),
    "] x [",
    paste0(ddn$contracting_dims[[2L]], collapse = ", "),
    "]",
    precision_repr,
    ": ",
    ctx$sig_str
  )
}

OpDotGeneral <- new_Op(
  "OpDotGeneral",
  "dot_general",
  render = render_dot_general
)

precision_values <- c("DEFAULT", "HIGH", "HIGHEST")

# Normalize a user-supplied precision config to a length-2 character vector
# (recycling a single value to both operands) or NULL.
normalize_precision_config <- function(precision_config) {
  if (is.null(precision_config)) {
    return(NULL)
  }
  if (length(precision_config) == 1L) {
    precision_config <- rep(precision_config, 2L)
  }
  precision_config
}

assert_precision_config <- function(precision_config) {
  if (is.null(precision_config)) {
    return(invisible(NULL))
  }
  # (C24) size(precision_config) = 2
  if (!is.character(precision_config) || length(precision_config) != 2L) {
    cli_abort(c(
      "{.arg precision_config} must be {.code NULL} or a character vector of length 2.",
      x = "Got an object of class {.cls {class(precision_config)}} with length {length(precision_config)}." # nolint
    ))
  }
  if (!all(precision_config %in% precision_values)) {
    cli_abort(c(
      "{.arg precision_config} entries must each be one of {.val {precision_values}}.", # nolint
      x = "Got {.val {precision_config}}."
    ))
  }
  invisible(NULL)
}

error_dot_general_dim_mismatch <- function(
  arg,
  shape_lhs,
  shape_rhs,
  type,
  dims,
  increment = 1L,
  call = sys.call(-1)[1L],
  class = character(),
  signal = TRUE
) {
  error_stablehlo(
    call = call,
    class = "ErrorDotGeneralDimMismatch",
    signal = signal,
    shape_lhs = shape_lhs,
    shape_rhs = shape_rhs,
    dims = dims,
    increment = increment,
    arg = arg
  )
}

#' @export
conditionMessage.ErrorDotGeneralDimMismatch <- function(c, ...) {
  format_error(
    c(
      "Can't perform dot general where shape(lhs) = {shapevec_repr(c$shape_lhs)} and shape(rhs) = {shapevec_repr(c$shape_rhs)} because the sizes of the {c$arg} don't match.", # nolint
      x = "They {c$arg} are {c$dims[[1L]]} for lhs and {c$dims[[2L]]} for rhs"
    ),
    .envir = environment()
  )
}

#' @param dot_dimension_numbers (`DotDimensionNumbers`)\cr
#'   The dot dimension number.
#' @param precision_config (`character()` | `NULL`)\cr
#'   The precision configuration, a character vector of length 2 giving the
#'   precision used for `lhs` and `rhs`. Each entry must be one of `"DEFAULT"`,
#'   `"HIGH"` or `"HIGHEST"`. If `NULL` (default), no precision configuration is
#'   emitted, which is equivalent to `"DEFAULT"` for both operands.
#' @rdname hlo_dot_general
#' @export
infer_types_dot_general <- function(
  lhs,
  rhs,
  dot_dimension_numbers,
  precision_config = NULL
) {
  if (!inherits(dot_dimension_numbers, "DotDimensionNumbers")) {
    cli_abort(c(
      "{.arg dot_dimension_numbers} must be a {.cls DotDimensionNumbers}.",
      x = "Got {.cls {class(dot_dimension_numbers)[1L]}}."
    ))
  }
  assert_precision_config(precision_config)
  assert_vts_are_tensors(lhs, rhs)
  # (C13)
  assert_vts_have_same_dtype(lhs, rhs)
  dim_lhs <- shape(lhs)
  dim_rhs <- shape(rhs)
  rank_lhs <- length(dim_lhs)
  rank_rhs <- length(dim_rhs)
  contracting_dims <- dot_dimension_numbers$contracting_dims
  batching_dims <- dot_dimension_numbers$batching_dims

  lhs_contracting <- contracting_dims[[1L]]
  rhs_contracting <- contracting_dims[[2L]]
  lhs_batching <- batching_dims[[1L]]
  rhs_batching <- batching_dims[[2L]]

  # (C1)
  if (length(lhs_batching) != length(rhs_batching)) {
    cli_abort(c(
      "{.arg batching_dims} must have equal length for {.arg lhs} and {.arg rhs}.",
      x = "Got lengths {length(lhs_batching)} and {length(rhs_batching)}."
    ))
  }

  # (C2)
  if (length(lhs_contracting) != length(rhs_contracting)) {
    cli_abort(c(
      "{.arg contracting_dims} must have equal length for {.arg lhs} and {.arg rhs}.",
      x = "Got lengths {length(lhs_contracting)} and {length(rhs_contracting)}."
    ))
  }

  # (C3)
  lhs_all_dims <- c(lhs_batching, lhs_contracting)
  if (anyDuplicated(lhs_all_dims)) {
    error_dimension_uniqueness(
      arg = "lhs batching_dims and contracting_dims",
      dimensions = lhs_all_dims
    )
  }

  # (C4)
  rhs_all_dims <- c(rhs_batching, rhs_contracting)
  if (anyDuplicated(rhs_all_dims)) {
    error_dimension_uniqueness(
      arg = "rhs batching_dims and contracting_dims",
      dimensions = rhs_all_dims
    )
  }

  # (C5)
  if (
    length(lhs_batching) > 0 &&
      (any(lhs_batching < 0L) || any(lhs_batching >= rank_lhs))
  ) {
    error_index_out_of_bounds(
      arg = "lhs_batching_dims",
      index = lhs_batching,
      lower = 0L,
      upper = rank_lhs
    )
  }

  # (C6)
  if (
    length(lhs_contracting) > 0 &&
      (any(lhs_contracting < 0L) || any(lhs_contracting >= rank_lhs))
  ) {
    error_index_out_of_bounds(
      arg = "lhs_contracting_dims",
      index = lhs_contracting,
      lower = 0L,
      upper = rank_lhs
    )
  }

  # (C7)
  if (
    length(rhs_batching) > 0 &&
      (any(rhs_batching < 0L) || any(rhs_batching >= rank_rhs))
  ) {
    error_index_out_of_bounds(
      arg = "rhs_batching_dims",
      index = rhs_batching,
      lower = 0L,
      upper = rank_rhs
    )
  }

  # (C8)
  if (
    length(rhs_contracting) > 0 &&
      (any(rhs_contracting < 0L) || any(rhs_contracting >= rank_rhs))
  ) {
    error_index_out_of_bounds(
      arg = "rhs_contracting_dims",
      index = rhs_contracting,
      lower = 0L,
      upper = rank_rhs
    )
  }

  # stableHLO uses 0-based indexing
  dim_merge1 <- dim_lhs[lhs_contracting + 1L]
  dim_merge2 <- dim_rhs[rhs_contracting + 1L]
  dim_batch1 <- dim_lhs[lhs_batching + 1L]
  dim_batch2 <- dim_rhs[rhs_batching + 1L]

  # (C10)
  if (!identical(dim_merge1, dim_merge2)) {
    error_dot_general_dim_mismatch(
      arg = "contracting_dims",
      shape_lhs = dim_lhs,
      shape_rhs = dim_rhs,
      dims = contracting_dims
    )
  }

  # (C9)
  if (!identical(dim_batch1, dim_batch2)) {
    error_dot_general_dim_mismatch(
      arg = "batching_dims",
      shape_lhs = dim_lhs,
      shape_rhs = dim_rhs,
      dims = batching_dims
    )
  }

  ii1 <- c(lhs_contracting, lhs_batching)
  dim_lhs_remaining <- if (length(ii1)) {
    dim_lhs[-(ii1 + 1L)]
  } else {
    dim_lhs
  }
  ii2 <- c(rhs_contracting, rhs_batching)
  dim_rhs_remaining <- if (length(ii2)) {
    dim_rhs[-(ii2 + 1L)]
  } else {
    dim_rhs
  }
  # C12
  out_dim <- c(dim_batch1, dim_lhs_remaining, dim_rhs_remaining)

  ValueTypes(list(
    ValueType(TensorType(dtype = lhs$type$dtype, shape = Shape(out_dim)))
  ))
}

#' @title DotDimensionNumbers
#' @description
#' Represents the dot dimension numbers.
#' @param contracting_dims (`integer()`)\cr
#'   The contracting dimensions.
#' @param batching_dims (`integer()` | `NULL`)\cr
#'   The batching dimensions.
#' @export
DotDimensionNumbers <- function(contracting_dims, batching_dims = NULL) {
  structure(
    list(contracting_dims = contracting_dims, batching_dims = batching_dims),
    class = "DotDimensionNumbers"
  )
}

dot_general_impl <- hlo_fn(OpDotGeneral, infer_types_dot_general)

#' @templateVar mnemonic dot_general
#' @templateVar not_func_variables precision_config
#' @template op
#' @export
hlo_dot_general <- function(
  lhs,
  rhs,
  contracting_dims,
  batching_dims = NULL,
  precision_config = NULL,
  output_types = NULL
) {
  precision_config <- normalize_precision_config(precision_config)
  dot_general_impl(
    values = list(
      lhs = lhs,
      rhs = rhs
    ),
    output_types = output_types,
    custom_attrs = list(
      dot_dimension_numbers = DotDimensionNumbers(
        contracting_dims = contracting_dims,
        batching_dims = batching_dims
      ),
      precision_config = precision_config
    )
  )
}

#' @export
repr.DotDimensionNumbers <- function(x, ...) {
  contracting <- paste0(
    "contracting_dims = [",
    paste0(x$contracting_dims[[1L]], collapse = ", "),
    "] x [",
    paste0(x$contracting_dims[[2L]], collapse = ", "),
    "]"
  )
  if (is.null(x$batching_dims)) {
    return(contracting)
  }
  paste0(
    "batching_dims = [",
    paste0(x$batching_dims[[1L]], collapse = ", "),
    "] x [",
    paste0(x$batching_dims[[2L]], collapse = ", "),
    "], ",
    contracting
  )
}
