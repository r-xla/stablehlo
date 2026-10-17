test_that("return cleans up afterwards", {
  f <- local_func("f")
  g <- (function() {
    local_func()
    x <- hlo_input("x", "f32", shape = c(2, 2))
    hlo_return(x)
  })()
  expect_equal(.current_func(), f)
})

test_that("double nested return", {
  a <- local_func("a")
  g <- (function() {
    b <- local_func("b")
    h <- (function() {
      local_func("c")
      hlo_return(hlo_tensor(1))
    })()
    expect_equal(.current_func(), b)
  })()
  expect_equal(.current_func(), a)
})

test_that("a token can be returned", {
  local_func()
  x <- hlo_input("x", "f32", shape = 2L)
  f <- hlo_return(x, hlo_after_all())
  expect_match(repr(f), "!stablehlo.token", fixed = TRUE)
})
