test_that("after_all works", {
  x <- hlo_input("x", "f32", shape = c(2L, 2L), "main")
  z <- hlo_after_all(x, .update_pointer = FALSE)
  func <- hlo_return(z[[1L]])
  expect_snapshot(repr(func))
})
