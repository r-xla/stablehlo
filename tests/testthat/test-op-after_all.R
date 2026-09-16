test_that("basic tests", {
  func <- local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  # (I1) `inputs` is a variadic of tokens, and the empty case is legal --
  # `after_all` with no inputs is how a token is produced in the first place.
  t1 <- hlo_after_all()
  t2 <- hlo_after_all(t1)
  result_func <- hlo_return(x)
  expect_snapshot(repr(result_func))
})

test_that("after_all rejects a non-token input", {
  # (I1) types `inputs` a "variadic number of token". A tensor here used to be
  # accepted and rendered, and only MLIR refused the program.
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  expect_error(hlo_after_all(x), "must be tokens")
})
