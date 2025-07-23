test_that("hlo_input works", {
  local_reset_id_gen()
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  f <- hlo_return(x)
  expect_snapshot(repr(f))
})

test_that("hlo_capture works", {
  local_reset_id_gen()
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  xcap <- hlo_capture(x)
  expect_list(xcap, types = "stablehlo::FuncVariable", len = 1L)
  expect_equal(xcap[[1L]]@value_id@id, x@value_id@id)
  expect_equal(xcap[[1L]]@value_type, x@value_type)
  expect_snapshot(repr(xcap[[1L]]@func))

  outs <- hlo_capture(
    hlo_input("y", "f32", shape = c(2L, 2L)),
    hlo_input("z", "f32", shape = c(2L, 2L))
  )
  y <- outs[[1L]]
  z <- outs[[2L]]
  expect_list(outs, types = "stablehlo::FuncVariable", len = 2L)
  expect_equal(outs[[1L]]@value_id@id, y@value_id@id)
  expect_equal(outs[[1L]]@value_type, y@value_type)
  expect_equal(outs[[2L]]@value_id@id, z@value_id@id)
  expect_equal(outs[[2L]]@value_type, z@value_type)

  expect_snapshot(repr(outs[[1L]]@func))
  expect_snapshot(repr(outs[[2L]]@func))
})
