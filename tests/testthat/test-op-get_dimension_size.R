test_that("static axis", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 3L))
  y <- hlo_get_dimension_size(x, dimension = 1L)
  f <- hlo_return(y)
  expect_match(repr(f), "stablehlo.get_dimension_size", fixed = TRUE)
  expect_match(repr(f), "tensor<i32>", fixed = TRUE)
})

test_that("dynamic axis", {
  local_func()
  x <- hlo_input("x", "f32", shape = Shape(NA_integer_))
  y <- hlo_get_dimension_size(x, dimension = 0L)
  f <- hlo_return(y)
  expect_match(repr(f), "tensor<?xf32>", fixed = TRUE)
})

test_that("the dimension must be in range", {
  local_func()
  x <- hlo_input("x", "f32", shape = 3L)
  expect_error(hlo_get_dimension_size(x, dimension = 1L))
})

# ---- dynamic axis sizes ----------------------------------------------------

test_that("scalar broadcast via get_dimension_size", {
  skip_if_no_refine()
  # The pair of ops this branch added: read the size off the value, then
  # broadcast to it. Refinement has to fold the read into a constant.
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      size <- hlo_reshape(hlo_get_dimension_size(a, dimension = 0L), shape = 1L)
      two <- hlo_dynamic_broadcast_in_dim(
        hlo_scalar(2, dtype = "f32"),
        size,
        broadcast_dimensions = integer(),
        shape = shapes[[1L]]
      )
      hlo_multiply(a, two)
    },
    dyn_shapes = list(N),
    runs = list(
      list(shapes = list(3L), args = list(c(1, 2, 3))),
      list(shapes = list(5L), args = list(1:5 + 0))
    )
  )
})
