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
