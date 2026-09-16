#' @include op.R hlo.R
NULL

OpDynamicBroadcastInDim <- new_Op(
  "OpDynamicBroadcastInDim",
  "dynamic_broadcast_in_dim"
)

#' @rdname hlo_dynamic_broadcast_in_dim
#' @param broadcast_dimensions (`integer()`)\cr
#'   Maps each axis of `operand` to an axis of the result.
#' @param shape (`integer()`)\cr
#'   The result's static shape, with `NA` at each axis whose size is only known
#'   at run time. It cannot be inferred from the operands, because
#'   `output_dimensions` is data.
#' @export
infer_types_dynamic_broadcast_in_dim <- function(
  operand,
  output_dimensions,
  broadcast_dimensions,
  shape,
  known_expanding_dimensions = NULL,
  known_nonexpanding_dimensions = NULL
) {
  assert_vt_is_tensor(operand)
  assert_vt_is_tensor(output_dimensions)
  assert_const(broadcast_dimensions, dtype = "i64", naxes = 1L)
  # Unlike the static op, `NA` is allowed here: it is what a dynamic axis is
  # spelled as, and the whole point of this op.
  assert_shapevec_dyn(shape)

  # (C7) `size(output_dimensions) = rank(result)`, plus I2's type.
  assert_size_operand(output_dimensions, length(shape))

  operand_dims <- shape(operand)
  result_dims <- as.integer(shape)
  bdims <- broadcast_dimensions$data

  # (C2)
  if (length(bdims) != length(operand_dims)) {
    cli_abort(c(
      "Length of {.arg broadcast_dimensions} must equal rank of {.arg operand}.",
      x = "Got {length(bdims)} broadcast_dimensions for operand of rank {length(operand_dims)}."
    ))
  }

  # (C3)
  if (any(bdims < 0L | bdims >= length(result_dims))) {
    error_index_out_of_bounds(
      arg = "broadcast_dimensions",
      index = bdims,
      lower = 0L,
      upper = length(result_dims)
    )
  }

  # (C4)
  if (anyDuplicated(bdims)) {
    error_dimension_uniqueness(arg = "broadcast_dimensions", dimensions = bdims)
  }

  # (C5) as for the static op, but only where both sizes are known: a dynamic
  # axis on either side is checked at run time, not here.
  #
  # (C5) is a disjunction, so it also *refines*: once `dim(operand, d)` is
  # known and provably not 1, the first disjunct is out and the second must
  # hold, which pins `dim(result, broadcast_dimensions[d])` to it. A `?` there
  # therefore becomes the operand's size, and the op stays static-shaped where
  # the operand is -- without this the caller's `shape` hint would be taken
  # verbatim and a size known at build time would be thrown away.
  for (d in seq_along(bdims)) {
    op_dim <- operand_dims[d]
    out_axis <- bdims[d] + 1L
    out_dim <- result_dims[out_axis]
    # *Possibly* 1, so possibly expanding: for a `?` operand this is TRUE
    # because nothing rules 1 out, which is what makes both uses below safe.
    op_may_expand <- !provably_ne(op_dim, 1L)
    if (provably_ne(op_dim, out_dim) && !op_may_expand) {
      error_dim_size_mismatch(
        arg1 = "operand",
        arg2 = "result",
        dim1 = d - 1L,
        dim2 = bdims[d],
        shape1 = operand_dims,
        shape2 = result_dims
      )
    }
    if (!op_may_expand) {
      # Cannot clash: the line above already refused a provable mismatch.
      result_dims[out_axis] <- unify_shapes(
        out_dim,
        op_dim,
        arg_a = "result",
        arg_b = "operand"
      )
    }
  }

  # (C9), (C10) `0 <= known_(non)expanding_dimensions < rank(operand)`, and
  # (C8) the two sets are disjoint and each free of repeats. All three are
  # fully static, so none of them defers.
  known <- list(
    known_expanding_dimensions = known_expanding_dimensions,
    known_nonexpanding_dimensions = known_nonexpanding_dimensions
  )
  known_dims <- Map(
    function(value, nm) {
      if (is.null(value)) {
        return(NULL)
      }
      assert_const(value, dtype = "i64", naxes = 1L)
      dims <- as.integer(value$data)
      if (any(dims < 0L | dims >= length(operand_dims))) {
        error_index_out_of_bounds(
          arg = nm,
          index = dims,
          lower = 0L,
          upper = length(operand_dims)
        )
      }
      dims
    },
    known,
    names(known)
  )
  # (C8) `is_unique(known_expanding_dimensions + known_nonexpanding_dimensions)`
  # -- `+` is concatenation here, so an axis may not repeat within either set
  # nor appear in both.
  combined <- unlist(known_dims, use.names = FALSE)
  if (length(combined) && anyDuplicated(combined)) {
    # Not `error_dimension_uniqueness()`: the interesting failure is an axis
    # named by *both* sets, and that condition can name only one argument.
    repeated <- unique(combined[duplicated(combined)])
    cli_abort(c(
      "{.arg known_expanding_dimensions} and
       {.arg known_nonexpanding_dimensions} must name disjoint axes, each at
       most once.",
      x = "Got {.val {index_vec(repeated)}} more than once."
    ))
  }

  # (C1)
  ValueTypes(list(
    ValueType(TensorType(
      dtype = operand$type$dtype,
      shape = Shape(result_dims)
    ))
  ))
}

hlo_dynamic_broadcast_in_dim_impl <- hlo_fn(
  OpDynamicBroadcastInDim,
  infer_types_dynamic_broadcast_in_dim
)

#' @templateVar mnemonic dynamic_broadcast_in_dim
#' @templateVar not_func_variables broadcast_dimensions,shape,known_expanding_dimensions,known_nonexpanding_dimensions
#' @template op
#' @details
#' Note that `shape` is a *claim*, not a check: nothing here can verify it,
#' since the sizes it describes are data. Where StableHLO can constant-fold the
#' size operands it will verify the claim itself and reject a wrong one
#' downstream.
#' @param known_expanding_dimensions,known_nonexpanding_dimensions (`integer()` | `NULL`)\cr
#'   Optional static knowledge about which axes of `operand` are broadcast
#'   (size 1 expanded to the result's size) and which are carried through
#'   unchanged. `NULL` -- the default -- leaves the attribute off, which
#'   StableHLO reads as "every axis is possibly expanding". The two sets must
#'   be disjoint and within `operand`'s axes.
#' @export
hlo_dynamic_broadcast_in_dim <- function(
  operand,
  output_dimensions,
  broadcast_dimensions,
  shape,
  known_expanding_dimensions = NULL,
  known_nonexpanding_dimensions = NULL,
  output_types = NULL
) {
  # Both are `OptionalAttr` in the ODS, so `NULL` leaves the attribute off
  # rather than emitting an empty array. StableHLO reads the two the same way;
  # omitting it just keeps the emitted IR free of noise.
  optional_dims <- function(name, value) {
    if (is.null(value)) {
      return(NULL)
    }
    value <- as.integer(value)
    list(constant_attr(name, value, dtype = "i64", shape = length(value)))
  }
  hlo_dynamic_broadcast_in_dim_impl(
    values = list(operand = operand, output_dimensions = output_dimensions),
    output_types = output_types,
    attrs = c(
      list(
        constant_attr(
          "broadcast_dimensions",
          as.integer(broadcast_dimensions),
          dtype = "i64",
          shape = length(broadcast_dimensions)
        )
      ),
      optional_dims("known_expanding_dimensions", known_expanding_dimensions),
      optional_dims(
        "known_nonexpanding_dimensions",
        known_nonexpanding_dimensions
      )
    ),
    custom_attrs = list(shape = as.integer(shape))
  )
}
