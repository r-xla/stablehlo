# ---- dynamic axis sizes ----------------------------------------------------

test_that("dynamic_iota", {
  expect_equal(
    inferred(function() {
      hlo_dynamic_iota(
        dyn_input("s", "i32", 1L),
        iota_dimension = 0L,
        dtype = "f32",
        shape = N
      )
    }),
    "tensor<?xf32>"
  )
  # A static hint is fine too -- the sizes are still supplied at run time.
  expect_equal(
    inferred(function() {
      hlo_dynamic_iota(
        dyn_input("s", "i32", 2L),
        iota_dimension = 1L,
        dtype = "i32",
        shape = c(2L, N)
      )
    }),
    "tensor<2x?xi32>"
  )
  local_func()
  expect_error(
    hlo_dynamic_iota(
      dyn_input("s", "i32", 1L),
      iota_dimension = 3L,
      dtype = "f32",
      shape = N
    ),
    "iota_dimension"
  )
  # output_shape must have one element per result axis.
  local_func()
  expect_error(
    hlo_dynamic_iota(
      dyn_input("s", "i32", 3L),
      iota_dimension = 0L,
      dtype = "f32",
      shape = c(N, N)
    ),
    "one element per axis"
  )
})
