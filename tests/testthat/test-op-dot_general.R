test_that("matmul", {
  lhs <- hlo_input("lhs", "f32", shape = c(5, 4), func_id = "main")
  rhs <- hlo_input("rhs", "f32", shape = c(4, 3))
  z <- hlo_dot_general(
    lhs,
    rhs,
    contracting_dims = list(1L, 0L),
    batching_dims = list(integer(), integer())
  )
  f <- hlo_return(z)
  cat(repr(f), "\n")

  pjrt_program <- pjrt_program(repr(f))
  exec <- pjrt_compile(pjrt_program)

  expect_equal(
    pjrt_execute(
      exec,
      pjrt_buffer(array(as.double(1:20), dim = c(5, 4))),
      pjrt_buffer(array(as.double(1:12), dim = c(4, 3)))
    ),
    pjrt_buffer(
      array(as.double(1:20), dim = c(5, 4)) %*%
        array(as.double(1:12), dim = c(4, 3))
    )
  )
})

if (FALSE) {
  program <- r"(
  func.func @main(
    %lhs: tensor<2x2x2xi64>,
    %rhs: tensor<2x2x2xi64>
  ) -> tensor<2x2x2xi64> {
      %result = stablehlo.dot_general %lhs, %rhs, batching_dims = [0] x [0], contracting_dims = [2] x [1]: (tensor<2x2x2xi64>, tensor<2x2x2xi64>) -> tensor<2x2x2xi64>
    "func.return"(%result): (tensor<2x2x2xi64>) -> ()
  }
  )"
  pjrt_compile(pjrt_program(program))
}
