test_that("basic tests", {
  hlo_test_biv(hlo_add, `+`)
})

test_that("error", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  y <- hlo_input("y", "f32")
  expect_snapshot(hlo_add(x, y), error = TRUE)
})
