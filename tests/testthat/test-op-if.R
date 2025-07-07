test_that("If operator works", {
  x = hlo_input("x", "f32", shape = integer())
  y = hlo_add(x, x)
  f1 <- hlo_return(y)
  y <- hlo_abs(x)
  f2 <- hlo_return(y)

  which <- hlo_input("x", "i1", integer())
  out <- hlo_if(which, .funcs = list(f1, f2))
  f <- hlo_return(out)
  expect_snapshot(f)
})
