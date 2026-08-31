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
