test_that("hlo_input works", {
  local_reset_id_gen()
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  f <- hlo_return(x)
  expect_snapshot(repr(f))
})
