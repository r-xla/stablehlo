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

test_that("dynamic_iota refines, compiles and runs", {
  skip_if_no_refine()
  expect_dynamic_op_runs(
    build = function() {
      a <- dyn_input("a", "f32", N)
      size <- hlo_reshape(hlo_get_dimension_size(a, dimension = 0L), shape = 1L)
      iota <- hlo_dynamic_iota(
        size,
        iota_dimension = 0L,
        dtype = "f32",
        shape = N
      )
      hlo_add(a, iota)
    },
    types = "tensor<4xf32>",
    args = list(pjrt::pjrt_buffer(rep(10, 4), dtype = "f32")),
    refined_type = "tensor<4xf32>",
    expected = c(10, 11, 12, 13)
  )
})
