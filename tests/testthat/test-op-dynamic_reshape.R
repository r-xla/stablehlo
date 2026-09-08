# ---- dynamic axis sizes ----------------------------------------------------

test_that("dynamic_reshape", {
  expect_equal(
    inferred(function() {
      hlo_dynamic_reshape(
        dyn_input("a", "f32", N),
        dyn_input("s", "i32", 2L),
        shape = c(N, 3L)
      )
    }),
    "tensor<?x3xf32>"
  )
  # Element counts known on both sides and unequal: still refused.
  local_func()
  expect_error(
    hlo_dynamic_reshape(
      dyn_input("a", "f32", 5L),
      dyn_input("s", "i32", 2L),
      shape = c(2L, 3L)
    ),
    "same number of elements"
  )
  # A dynamic axis on either side defers that check -- the point of the op.
  expect_equal(
    inferred(function() {
      hlo_dynamic_reshape(
        dyn_input("a", "f32", N),
        dyn_input("s", "i32", 2L),
        shape = c(2L, 3L)
      )
    }),
    "tensor<2x3xf32>"
  )
})
