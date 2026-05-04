#' @include op.R hlo.R
NULL

#' @title Scan (left-/right-fold along a dimension)
#' @description
#' High-level helper that constructs IR for a scan along a single dimension.
#' At each step the user-supplied `body` closure receives the current carry
#' and a slice of each input along `dim`, and returns the updated carry and
#' an output element. Output elements are stacked back along `dim`, producing
#' a result with the same shape as `inputs[[1]]`.
#'
#' Internally `hlo_scan` decomposes into a [stablehlo.while][hlo_while()]
#' loop with `dynamic_slice` reads and `dynamic_update_slice` writes — there
#' is no dedicated `scan` op in StableHLO. The resulting IR runs on any
#' backend that implements `while`, but execution is sequential along `dim`.
#'
#' @param inputs (`list()` of [`FuncValue`])\cr
#'   One or more tensors, all sharing the same shape. The scan iterates over
#'   `dim`; per-iteration each input contributes a slice with `dim` removed.
#' @param init ([`FuncValue`])\cr
#'   The initial carry. Its `ValueType` is the carry type for every iteration
#'   and the carry returned at the end of the scan.
#' @param body (`function`)\cr
#'   Body closure called once during IR construction with signature
#'   `function(carry, ...elems) -> list(new_carry, out_elem)`:
#'   * `carry` has the same `ValueType` as `init`.
#'   * `elems...` are slices: one per element of `inputs`, each with `dim`
#'     removed from its shape and the dtype of the matching input.
#'   * `new_carry` must match `init`'s `ValueType`.
#'   * `out_elem` must have shape equal to the slice shape (i.e. `inputs[[1]]`'s
#'     shape with `dim` removed) and dtype matching `inputs[[1]]`.
#' @param dim (`integer(1)`)\cr
#'   Zero-based axis to scan over.
#' @param reverse (`logical(1)`)\cr
#'   If `TRUE`, the scan walks `dim` from `n - 1` down to `0`. Default `FALSE`.
#' @return ([`FuncValue`])\cr
#'   The stacked outputs, with the same shape and dtype as `inputs[[1]]`.
#' @export
hlo_scan <- function(inputs, init, body, dim, reverse = FALSE) {
  inputs <- ensure_func_vals(inputs)
  if (!test_class(init, "FuncValue")) {
    cli_abort("{.arg init} must be a {.cls FuncValue}.")
  }
  if (!is.function(body)) {
    cli_abort("{.arg body} must be a function.")
  }
  checkmate::assert_flag(reverse)

  if (length(inputs) == 0L) {
    cli_abort("{.arg inputs} must be a non-empty list of {.cls FuncValue}s.")
  }

  ref_shape <- shape(inputs[[1L]])
  rank <- length(ref_shape)
  if (rank == 0L) {
    cli_abort("{.arg inputs} must have rank >= 1.")
  }
  for (i in seq_along(inputs)[-1L]) {
    if (!identical(shape(inputs[[i]]), ref_shape)) {
      cli_abort(c(
        "All {.arg inputs} must share a shape.",
        x = "Input 1 has shape {shapevec_repr(ref_shape)}, input {i} has shape {shapevec_repr(shape(inputs[[i]]))}."
      ))
    }
  }
  dim <- assert_int(dim, coerce = TRUE, lower = 0L, upper = rank - 1L)

  n <- ref_shape[[dim + 1L]]
  # Use string dtypes throughout; hlo_input / ValueType / impl_hlo_constant
  # all accept strings via make_vt.
  output_dtype_str <- as.character(inputs[[1L]]$value_type$type$dtype)
  output_full_shape <- ref_shape
  slice_full_shape <- ref_shape
  slice_full_shape[[dim + 1L]] <- 1L
  reduced_shape <- ref_shape[-(dim + 1L)]
  carry_dtype_str <- as.character(init$value_type$type$dtype)
  carry_shape <- shape(init)
  input_dtype_strs <- vapply(
    inputs,
    function(in_) as.character(in_$value_type$type$dtype),
    character(1L)
  )
  num_inputs <- length(inputs)

  parent_func <- .current_func()

  # Pre-allocate the output buffer (zeros) and the loop counter / n in the
  # parent func.
  output_init <- impl_hlo_constant(
    rep(0, prod(output_full_shape)),
    dtype = output_dtype_str,
    func = parent_func,
    shape = output_full_shape
  )
  i_init <- impl_hlo_constant(0L, dtype = "i32", func = parent_func, shape = integer())
  n_const <- impl_hlo_constant(as.integer(n), dtype = "i32", func = parent_func, shape = integer())

  # Helper: declare loop-carry inputs in the order
  # (i, buf, carry, n, input_0, input_1, ..., input_{M-1}).
  # Used identically inside cond and body so their argument lists match.
  declare_loop_inputs <- function() {
    list(
      i      = hlo_input("i",     "i32",            integer()),
      buf    = hlo_input("buf",   output_dtype_str, output_full_shape),
      carry  = hlo_input("carry", carry_dtype_str,  carry_shape),
      n      = hlo_input("n",     "i32",            integer()),
      inputs = lapply(seq_len(num_inputs), function(k) {
        hlo_input(paste0("input_", k - 1L), input_dtype_strs[[k]], ref_shape)
      })
    )
  }

  # ---- cond: i < n -----------------------------------------------------------
  cond_func <- local_func("scan_cond")
  c_args <- declare_loop_inputs()
  c_pred <- hlo_compare(c_args$i, c_args$n, comparison_direction = "LT", compare_type = "SIGNED")
  cond_func <- hlo_return(c_pred)

  # ---- body ------------------------------------------------------------------
  body_func <- local_func("scan_body")
  b_args <- declare_loop_inputs()

  # Loop-position-to-actual-index along dim:
  #   forward: actual = i
  #   reverse: actual = (n - 1) - i
  if (reverse) {
    one_i32 <- hlo_scalar(1L, dtype = "i32")
    actual_idx <- hlo_subtract(hlo_subtract(b_args$n, one_i32), b_args$i)
  } else {
    actual_idx <- b_args$i
  }

  zero_idx <- hlo_scalar(0L, dtype = "i32")
  start_indices_axis_form <- lapply(seq_len(rank), function(ax) {
    if (ax - 1L == dim) actual_idx else zero_idx
  })

  # Slice each input along dim and drop the dim axis (size 1).
  elem_args <- lapply(b_args$inputs, function(in_full) {
    sliced <- rlang::exec(
      hlo_dynamic_slice,
      operand = in_full,
      !!!start_indices_axis_form,
      slice_sizes = slice_full_shape
    )
    hlo_reshape(sliced, reduced_shape)
  })

  # Invoke the user's body for (new_carry, out_elem).
  body_result <- do.call(body, c(list(b_args$carry), elem_args))
  if (!test_list(body_result, len = 2L)) {
    cli_abort(c(
      "{.arg body} must return a list of length 2 (new_carry, out_elem).",
      x = "Got {length(body_result)} element{?s}."
    ))
  }
  new_carry <- body_result[[1L]]
  out_elem <- body_result[[2L]]
  if (!test_class(new_carry, "FuncValue") || !test_class(out_elem, "FuncValue")) {
    cli_abort("{.arg body} must return a list of two {.cls FuncValue}s.")
  }
  if (new_carry$value_type != b_args$carry$value_type) {
    cli_abort(c(
      "{.arg body}'s first return value (new_carry) must match {.arg init}'s ValueType.",
      x = "Got new_carry: {.val {new_carry$value_type}}, init: {.val {b_args$carry$value_type}}."
    ))
  }
  expected_out_vt <- ValueType(output_dtype_str, shape = reduced_shape)
  if (out_elem$value_type != expected_out_vt) {
    cli_abort(c(
      "{.arg body}'s second return value (out_elem) must have inputs[[1]]'s shape minus {.arg dim} and dtype.",
      x = "Got out_elem: {.val {out_elem$value_type}}, expected: {.val {expected_out_vt}}."
    ))
  }

  # Lift back to slice_full_shape (size-1 axis at dim) and write into buffer.
  out_elem_full <- hlo_reshape(out_elem, slice_full_shape)
  new_buf <- rlang::exec(
    hlo_dynamic_update_slice,
    operand = b_args$buf,
    update = out_elem_full,
    !!!start_indices_axis_form
  )

  one_step <- hlo_scalar(1L, dtype = "i32")
  new_i <- hlo_add(b_args$i, one_step)

  body_func <- rlang::exec(
    hlo_return,
    new_i, new_buf, new_carry, b_args$n, !!!b_args$inputs
  )

  # ---- while op (in parent_func) ---------------------------------------------
  while_outs <- rlang::exec(
    hlo_while,
    i_init, output_init, init, n_const, !!!inputs,
    cond = cond_func,
    body = body_func,
    simplify = FALSE
  )
  while_outs[[2L]]
}
