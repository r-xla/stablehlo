test_that("basic tests", {
  hlo_test_biv(hlo_add, `+`)
})

# error tests for infer_types_generic_biv are in test-type_inference.R

# ---- dynamic axis sizes ----------------------------------------------------

test_that("a dynamic operand unifies with a static one", {
  expect_equal(
    inferred(function() {
      hlo_add(dyn_input("a", "f32", N), dyn_input("b", "f32", N))
    }),
    "tensor<?xf32>"
  )
  # Refinement: the known side wins, so everything downstream stays static.
  expect_equal(
    inferred(function() {
      hlo_add(dyn_input("a", "f32", N), dyn_input("b", "f32", 3L))
    }),
    "tensor<3xf32>"
  )
})

test_that("a definite size clash is still an error", {
  local_func()
  expect_error(
    hlo_add(dyn_input("a", "f32", 3L), dyn_input("b", "f32", 4L)),
    "same tensor type"
  )
})

test_that("a dynamic operand mixed with a static one refines and runs", {
  skip_if_no_refine()
  # The second argument's shape is static in the program, so refinement only
  # has the first to resolve -- and our inference already refined the result to
  # the static shape.
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      b <- dyn_input("b", "f32", shapes[[2L]])
      hlo_add(a, b)
    },
    dyn_shapes = list(N, 4L),
    runs = list(list(shapes = list(4L, 4L), args = list(1:4 + 0, rep(10, 4))))
  )
})

test_that("one dynamic executable runs at more than one size", {
  skip_if_no_iree_compile()

  # The whole point of the feature, as one assertion: compile *once* against
  # `tensor<?xf32>`, then run the same executable at three different sizes.
  local_func(id = "main")
  a <- dyn_input("a", "f32", N)
  b <- dyn_input("b", "f32", N)
  src <- repr(hlo_return(hlo_add(a, b)))
  expect_match(src, "tensor<?xf32>", fixed = TRUE)

  for (n in c(3L, 5L, 1L)) {
    x <- as.double(seq_len(n))
    v <- paste(x, collapse = " ")
    got <- iree_run(src, rep(sprintf("%dxf32=%s", n, v), 2L))
    expect_equal(got, x + x, tolerance = 1e-6, info = paste("n =", n))
  }
})

test_that("a refined type still runs, and inference did not lie", {
  skip_if_no_iree_compile()

  # `add(tensor<?xf32>, tensor<3xf32>)` infers `tensor<3xf32>`: inference
  # claims the dynamic side must be 3. Check the claim against a runtime that
  # can see the sizes, rather than trusting the inference that produced it.
  local_func(id = "main")
  a <- dyn_input("a", "f32", N)
  b <- dyn_input("b", "f32", 3L)
  out <- hlo_add(a, b)
  expect_equal(repr(out$value_type$type), "tensor<3xf32>")

  got <- iree_run(
    repr(hlo_return(out)),
    c("3xf32=1 2 3", "3xf32=10 20 30")
  )
  expect_equal(got, c(11, 22, 33), tolerance = 1e-6)
})
