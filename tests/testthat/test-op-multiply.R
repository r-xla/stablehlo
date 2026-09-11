test_that("basic tests", {
  hlo_test_biv(hlo_multiply, `*`)
})

# Errors are tested in test-type_inference.R (via infer_types_generic_biv)

# ---- dynamic axis sizes ----------------------------------------------------

test_that("dynamic and static axis sizes meet axis by axis", {
  expect_equal(
    inferred(function() {
      hlo_multiply(
        dyn_input("a", "f32", c(N, 3L)),
        dyn_input("b", "f32", c(2L, N))
      )
    }),
    "tensor<2x3xf32>"
  )
})

test_that("a chain of elementwise ops keeps its dynamic axis and runs", {
  skip_if_no_refine()
  expect_refines_and_runs(
    build = function(shapes) {
      a <- dyn_input("a", "f32", shapes[[1L]])
      b <- dyn_input("b", "f32", shapes[[2L]])
      hlo_multiply(hlo_add(a, b), a)
    },
    dyn_shapes = list(N, N),
    runs = list(
      list(shapes = list(3L, 3L), args = list(c(1, 2, 3), c(10, 20, 30))),
      list(shapes = list(5L, 5L), args = list(1:5 + 0, rep(2, 5)))
    )
  )
})
