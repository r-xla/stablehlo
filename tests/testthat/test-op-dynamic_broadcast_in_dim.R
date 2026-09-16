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
    types = list(list("f32", 4)),
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

test_that("dynamic_broadcast_in_dim pins a result axis its operand determines", {
  # (C5) is a disjunction: once `dim(operand, d)` is known and provably not 1,
  # the "operand dim is 1" branch is out and the result axis must equal it. So
  # a `?` in the hint becomes the operand's size rather than surviving into
  # the result type.
  expect_equal(
    inferred(function() {
      hlo_dynamic_broadcast_in_dim(
        dyn_input("a", "f32", 3L),
        dyn_input("s", "i64", 1L),
        broadcast_dimensions = 0L,
        shape = N
      )
    }),
    "tensor<3xf32>"
  )
  # An operand axis of 1 may expand to anything, so it pins nothing.
  expect_equal(
    inferred(function() {
      hlo_dynamic_broadcast_in_dim(
        dyn_input("a", "f32", 1L),
        dyn_input("s", "i64", 1L),
        broadcast_dimensions = 0L,
        shape = N
      )
    }),
    "tensor<?xf32>"
  )
  # Nor does a dynamic operand axis, which could turn out to be 1.
  expect_equal(
    inferred(function() {
      hlo_dynamic_broadcast_in_dim(
        dyn_input("a", "f32", N),
        dyn_input("s", "i64", 1L),
        broadcast_dimensions = 0L,
        shape = N
      )
    }),
    "tensor<?xf32>"
  )
})

test_that("dynamic_broadcast_in_dim validates the known-expanding attributes", {
  bcast <- function(...) {
    hlo_dynamic_broadcast_in_dim(
      dyn_input("a", "f32", c(1L, 3L)),
      dyn_input("s", "i64", 2L),
      broadcast_dimensions = c(0L, 1L),
      shape = c(N, 3L),
      ...
    )
  }
  # Both are optional, and omitting them leaves the attribute off entirely --
  # `OptionalAttr` in the ODS, and "absent" does not mean "empty".
  local_func()
  expect_false(grepl("known_expanding", repr(hlo_return(bcast()))))

  src <- local({
    local_func()
    repr(hlo_return(bcast(
      known_expanding_dimensions = 0L,
      known_nonexpanding_dimensions = 1L
    )))
  })
  expect_match(src, "known_expanding_dimensions = array<i64: 0>", fixed = TRUE)
  expect_match(
    src,
    "known_nonexpanding_dimensions = array<i64: 1>",
    fixed = TRUE
  )

  # (C9)/(C10) `0 <= known_*_dimensions < rank(operand)`.
  local_func()
  expect_error(
    bcast(known_expanding_dimensions = 2L),
    class = "ErrorIndexOutOfBounds"
  )
  # (C8) the two sets are disjoint and each free of repeats.
  local_func()
  expect_error(
    bcast(
      known_expanding_dimensions = 0L,
      known_nonexpanding_dimensions = 0L
    ),
    "disjoint axes"
  )
})
