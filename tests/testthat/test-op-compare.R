test_that("simple test", {
  local_func()
  lhs <- hlo_input("lhs", "f32", shape = integer())
  rhs <- hlo_input("rhs", "f32", shape = integer())

  result <- hlo_compare(
    lhs,
    rhs,
    comparison_direction = "LT",
    compare_type = "FLOAT"
  )
  f <- hlo_return(result)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")

  program <- pjrt_program(repr(f))
  executable <- pjrt_compile(program)

  x <- array(c(1, 2, 3, 4), dim = c(2, 2))
  x_buf <- pjrt_buffer(x)
  out_buf <- pjrt_execute(
    executable,
    pjrt_scalar(1),
    pjrt_scalar(2)
  )
  expect_equal(out_buf, pjrt_scalar(TRUE))
})

test_that("exhaustive test", {
  # Define all combinations of comparison directions and types
  directions <- c("EQ", "NE", "GE", "GT", "LE", "LT")
  types <- c("FLOAT", "SIGNED", "UNSIGNED")

  # Create mapping of comparison functions
  compare_functions <- list(
    "EQ" = `==`,
    "NE" = `!=`,
    "GE" = `>=`,
    "GT" = `>`,
    "LE" = `<=`,
    "LT" = `<`
  )

  # Test float comparisons
  for (direction in directions) {
    hlo_test_biv(
      hlo_compare,
      compare_functions[[direction]],
      dtype = "f32",
      args = list(
        comparison_direction = direction,
        compare_type = "FLOAT"
      ),
      snapshot = FALSE
    )
  }

  # Test signed integer comparisons
  for (direction in directions) {
    hlo_test_biv(
      hlo_compare,
      compare_functions[[direction]],
      dtype = "i32",
      args = list(
        comparison_direction = direction,
        compare_type = "SIGNED"
      ),
      snapshot = FALSE
    )
  }

  # Test unsigned integer comparisons
  for (direction in directions) {
    hlo_test_biv(
      hlo_compare,
      compare_functions[[direction]],
      dtype = "ui32",
      args = list(
        comparison_direction = direction,
        compare_type = "UNSIGNED"
      ),
      snapshot = FALSE
    )
  }

  # Test boolean comparisons
  for (direction in c("EQ", "NE")) {
    # Only EQ and NE make sense for booleans
    hlo_test_biv(
      hlo_compare,
      compare_functions[[direction]],
      dtype = "pred",
      args = list(
        comparison_direction = direction,
        compare_type = "UNSIGNED"
      ),
      snapshot = FALSE
    )
  }
})
