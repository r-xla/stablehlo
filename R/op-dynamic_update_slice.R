#' @include op.R hlo.R
NULL

OpDynamicUpdateSlice <- new_Op("OpDynamicUpdateSlice", "dynamic_update_slice")

infer_types_dynamic_update_slice <- function(
  operand,
  update,
  ...
) {
  stopifnot(inherits(operand@type, TensorType))
  stopifnot(inherits(update@type, TensorType))

  operand_shape <- shape(operand)
  update_shape <- shape(update)

  # (C2) element_type(update) = element_type(operand)
  if (operand@type@dtype != update@type@dtype) {
    cli_abort("Element types of operand and update must match")
  }

  # (C3) rank(update) = rank(operand)
  if (length(update_shape) != length(operand_shape)) {
    cli_abort("Ranks of operand and update must match")
  }

  # Collect variadic start indices
  start_indices <- list(...)

  # (C4) size(start_indices) = rank(operand)
  if (length(start_indices) != length(operand_shape)) {
    cli_abort("Number of start_indices must equal rank of operand")
  }

  # (C5) same(type(start_indices...)) and each is a 0-D integer tensor
  if (length(start_indices) > 0L) {
    ref_type <- start_indices[[1L]]@type
    for (si in start_indices) {
      stopifnot(inherits(si@type, TensorType))
      # 0-dimensional
      if (length(si@type@shape@dims) != 0L) {
        cli_abort("Each start index must be a 0-dimensional tensor")
      }
      # integer dtype
      if (
        !inherits(si@type@dtype, IntegerType) &&
          !inherits(si@type@dtype, UnsignedType)
      ) {
        cli_abort("start_indices must have integer tensor type")
      }
      # same type
      if (!(si@type == ref_type)) {
        cli_abort("All start_indices must have the same type")
      }
    }
  }

  # (C6) 0 <= shape(update) <= shape(operand)
  if (length(update_shape) > 0L) {
    if (any(update_shape < 0L)) {
      cli_abort("update shape must be non-negative")
    }
    if (any(update_shape > operand_shape)) {
      cli_abort(
        "Each update dimension must be <= corresponding operand dimension"
      )
    }
  }

  # (C1) type(result) = type(operand)
  ValueTypes(list(ValueType(operand@type)))
}

hlo_dynamic_update_slice_impl <- hlo_fn(
  OpDynamicUpdateSlice,
  infer_types_dynamic_update_slice
)

#' @templateVar mnemonic dynamic_update_slice
#' @template op
#' @export
hlo_dynamic_update_slice <- function(
  operand,
  update,
  start_indices
) {
  # start_indices is an integer vector; materialize as 0-D i64 tensor values
  si_values <- lapply(as.integer(start_indices), function(idx) {
    hlo_scalar(idx, dtype = "i64", func = .current_func())
  })

  hlo_dynamic_update_slice_impl(
    values = c(list(operand = operand, update = update), si_values)
  )
}

method(repr, OpDynamicUpdateSlice) <- function(x) {
  paste0(
    repr(x@outputs),
    " = ",
    repr(x@name),
    " ",
    repr(x@inputs, simplify_dense = TRUE),
    ": ",
    repr(x@signature)
  )
}
