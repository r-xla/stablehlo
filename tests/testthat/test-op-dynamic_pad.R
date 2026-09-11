# ---- dynamic axis sizes ----------------------------------------------------

test_that("dynamic_pad", {
  expect_equal(
    inferred(function() {
      hlo_dynamic_pad(
        dyn_input("a", "f32", N),
        hlo_scalar(0, dtype = "f32"),
        dyn_input("lo", "i32", 1L),
        dyn_input("hi", "i32", 1L),
        dyn_input("in", "i32", 1L),
        shape = N
      )
    }),
    "tensor<?xf32>"
  )
  # The padding vectors need one element per operand axis.
  local_func()
  expect_error(
    hlo_dynamic_pad(
      dyn_input("a", "f32", c(N, 3L)),
      hlo_scalar(0, dtype = "f32"),
      dyn_input("lo", "i32", 1L),
      dyn_input("hi", "i32", 2L),
      dyn_input("in", "i32", 2L),
      shape = c(N, N)
    ),
    "one element per axis"
  )
  # padding_value must be a scalar of the operand's type.
  local_func()
  expect_error(
    hlo_dynamic_pad(
      dyn_input("a", "f32", N),
      dyn_input("pv", "f64", integer()),
      dyn_input("lo", "i32", 1L),
      dyn_input("hi", "i32", 1L),
      dyn_input("in", "i32", 1L),
      shape = N
    ),
    "same data type"
  )
})

test_that("dynamic_pad refines, compiles and runs", {
  skip_if_no_refine()
  expect_dynamic_op_runs(
    build = function() {
      a <- dyn_input("a", "f32", N)
      one <- hlo_tensor(1L, dtype = "i32", shape = 1L)
      zero <- hlo_tensor(0L, dtype = "i32", shape = 1L)
      hlo_dynamic_pad(
        a,
        hlo_scalar(0, dtype = "f32"),
        one,
        one,
        zero,
        shape = 5L
      )
    },
    types = "tensor<3xf32>",
    args = list(pjrt::pjrt_buffer(c(1, 2, 3), dtype = "f32")),
    inferred_type = "tensor<5xf32>",
    refined_type = "tensor<5xf32>",
    expected = c(0, 1, 2, 3, 0)
  )
})

test_that("the padding operands must be integers of one identical type", {
  dp <- function(lo, hi, inter) {
    local_func()
    hlo_dynamic_pad(
      dyn_input("a", "f32", 4L),
      hlo_scalar(0, dtype = "f32"),
      dyn_input("lo", lo, 1L),
      dyn_input("hi", hi, 1L),
      dyn_input("in", inter, 1L),
      shape = N
    )
  }
  expect_error(dp("f32", "i32", "i32"), "must have dtype int or uint")
  # (C2) `size(edge_padding_low) = size(edge_padding_high) = ...`, which the
  # ODS sharpens to one identical type.
  expect_error(dp("i32", "i64", "i32"), "same type")
  expect_no_error(dp("i32", "i32", "i32"))
})

test_that("the padding operands' own extent cannot be dynamic", {
  # StableHLO types them statically shaped, so a `tensor<?xi32>` here could
  # not be satisfied at any run-time size -- it is not something to defer.
  local_func()
  expect_error(
    hlo_dynamic_pad(
      dyn_input("a", "f32", 4L),
      hlo_scalar(0, dtype = "f32"),
      dyn_input("lo", "i32", N),
      dyn_input("hi", "i32", N),
      dyn_input("in", "i32", N),
      shape = N
    ),
    "statically known number of elements"
  )
})

test_that("the result takes the shape hint, not a blanket dynamic shape", {
  # Every other assertion here passes a fully dynamic hint, so an op that
  # ignored `shape` and returned all-`?` would satisfy them all. A static hint
  # is what pins the result to the hint.
  expect_equal(
    inferred(function() {
      hlo_dynamic_pad(
        dyn_input("a", "f32", N),
        hlo_scalar(0, dtype = "f32"),
        dyn_input("lo", "i32", 1L),
        dyn_input("hi", "i32", 1L),
        dyn_input("in", "i32", 1L),
        shape = 5L
      )
    }),
    "tensor<5xf32>"
  )
})
