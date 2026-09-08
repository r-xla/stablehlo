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

test_that("dynamic_reshape refines, compiles and runs", {
  skip_if_no_refine()
  expect_dynamic_op_runs(
    build = function() {
      a <- dyn_input("a", "f32", c(N, 3L))
      hlo_dynamic_reshape(
        a,
        hlo_tensor(6L, dtype = "i32", shape = 1L),
        shape = 6L
      )
    },
    types = "tensor<2x3xf32>",
    args = list(pjrt::pjrt_buffer(1:6 + 0, dtype = "f32", shape = c(2L, 3L))),
    refined_type = "tensor<6xf32>",
    expected = c(1, 3, 5, 2, 4, 6)
  )
})
