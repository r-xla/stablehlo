test_that("optimization_barrier looks correct", {
  # snapshot tests don't work in `it()` blocks
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 3L))
  y <- hlo_input("y", "f32", shape = c(2L, 3L))
  out <- hlo_optimization_barrier(x, y)
  f <- hlo_return(out[[1L]], out[[2L]])
  expect_snapshot(repr(f))
})

describe("hlo_optimization_barrier", {
  run <- function(f, ...) {
    executable <- pjrt::pjrt_compile(pjrt::pjrt_program(repr(f)))
    pjrt::pjrt_execute(executable, ...)
  }

  it("returns operands of different types unchanged", {
    skip_if_no_pjrt()
    local_func()
    x <- hlo_input("x", "f32", shape = c(2L, 3L))
    y <- hlo_input("y", "i32", shape = integer())
    out <- hlo_optimization_barrier(x, y)
    f <- hlo_return(out[[1L]], out[[2L]])
    x_buf <- pjrt::pjrt_buffer(matrix(1:6 / 2, 2L, 3L), dtype = "f32")
    y_buf <- pjrt::pjrt_scalar(7L, dtype = "i32")
    res <- run(f, x_buf, y_buf)
    expect_equal(res[[1L]], x_buf)
    expect_equal(res[[2L]], y_buf)
  })

  it("returns operands of one type unchanged", {
    skip_if_no_pjrt()
    local_func()
    x <- hlo_input("x", "f32", shape = 2L)
    y <- hlo_input("y", "f32", shape = 2L)
    z <- hlo_input("z", "f32", shape = 2L)
    out <- hlo_optimization_barrier(x, y, z)
    f <- hlo_return(out[[1L]], out[[2L]], out[[3L]])
    bufs <- lapply(1:3, function(i) pjrt::pjrt_buffer(c(i, -i), dtype = "f32"))
    res <- do.call(run, c(list(f), bufs))
    expect_equal(res, bufs)
  })

  it("takes tokens", {
    skip_if_no_pjrt()
    local_func()
    x <- hlo_input("x", "f32", shape = 2L)
    out <- hlo_optimization_barrier(x, hlo_after_all(), hlo_after_all())
    expect_equal(out[[2L]]$value_type, ValueType(TokenType()))
    f <- hlo_return(out[[1L]])
    x_buf <- pjrt::pjrt_buffer(c(1, 2), dtype = "f32")
    expect_equal(run(f, x_buf), x_buf)
  })

  it("requires at least one operand", {
    local_func()
    expect_error(hlo_optimization_barrier(), "at least one operand")
  })

  it("gives a single value for a single operand unless simplify = FALSE", {
    local_func()
    x <- hlo_input("x", "f32", shape = 2L)
    expect_s3_class(hlo_optimization_barrier(x), "FuncValue")
    out <- hlo_optimization_barrier(x, simplify = FALSE)
    expect_length(out, 1L)
    expect_s3_class(out[[1L]], "FuncValue")
  })
})
