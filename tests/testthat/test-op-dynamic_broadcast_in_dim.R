test_that("broadcasts a scalar to a dynamic shape", {
  local_func()
  x <- hlo_input("x", "f32", shape = Shape(NA_integer_))
  size <- hlo_reshape(hlo_get_dimension_size(x, dimension = 0L), shape = 1L)
  s <- hlo_scalar(2, dtype = "f32")
  b <- hlo_dynamic_broadcast_in_dim(
    s,
    size,
    broadcast_dimensions = integer(),
    shape = NA_integer_
  )
  f <- hlo_return(hlo_multiply(x, b))
  src <- repr(f)
  expect_match(src, "stablehlo.dynamic_broadcast_in_dim", fixed = TRUE)
  expect_match(
    src,
    "(tensor<f32>, tensor<1xi32>) -> (tensor<?xf32>)",
    fixed = TRUE
  )
})

test_that("a static target gives a static result type", {
  local_func()
  x <- hlo_input("x", "f32", shape = 3L)
  size <- hlo_reshape(hlo_get_dimension_size(x, dimension = 0L), shape = 1L)
  s <- hlo_scalar(2, dtype = "f32")
  b <- hlo_dynamic_broadcast_in_dim(
    s,
    size,
    broadcast_dimensions = integer(),
    shape = 3L
  )
  expect_equal(shape(b$value_type$type), 3L)
})

test_that("broadcast_dimensions is checked", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 3L))
  size <- hlo_input("s", "i32", shape = 2L)
  expect_error(
    hlo_dynamic_broadcast_in_dim(
      x,
      size,
      broadcast_dimensions = 0L,
      shape = c(NA_integer_, 3L)
    ),
    "must equal rank"
  )
  expect_error(
    hlo_dynamic_broadcast_in_dim(
      x,
      size,
      broadcast_dimensions = c(0L, 0L),
      shape = c(NA_integer_, 3L)
    ),
    class = "ErrorDimensionUniqueness"
  )
})

test_that("dynamic_broadcast_in_dim refines, compiles and runs", {
  skip_if_no_refine()
  expect_dynamic_op_runs(
    build = function() {
      a <- dyn_input("a", "f32", N)
      size <- hlo_reshape(hlo_get_dimension_size(a, dimension = 0L), shape = 1L)
      two <- hlo_dynamic_broadcast_in_dim(
        hlo_scalar(2, dtype = "f32"),
        size,
        broadcast_dimensions = integer(),
        shape = N
      )
      hlo_multiply(a, two)
    },
    types = "tensor<4xf32>",
    args = list(pjrt::pjrt_buffer(c(1, 2, 3, 4), dtype = "f32")),
    inferred_type = "tensor<?xf32>",
    refined_type = "tensor<4xf32>",
    expected = c(2, 4, 6, 8)
  )
})

test_that("output_dimensions must be an integer tensor of static extent", {
  local_func()
  expect_error(
    hlo_dynamic_broadcast_in_dim(
      dyn_input("a", "f32", 1L),
      dyn_input("s", "f32", 1L),
      broadcast_dimensions = 0L,
      shape = N
    ),
    "must have dtype int or uint"
  )
  local_func()
  expect_error(
    hlo_dynamic_broadcast_in_dim(
      dyn_input("a", "f32", 1L),
      dyn_input("s", "i64", N),
      broadcast_dimensions = 0L,
      shape = N
    ),
    "statically known number of elements"
  )
})

test_that("the result takes the shape hint, not a blanket dynamic shape", {
  expect_equal(
    inferred(function() {
      hlo_dynamic_broadcast_in_dim(
        dyn_input("a", "f32", 1L),
        dyn_input("s", "i64", 2L),
        broadcast_dimensions = 0L,
        shape = c(1L, 3L)
      )
    }),
    "tensor<1x3xf32>"
  )
})
