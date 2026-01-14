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
      dtypes = "f32",
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
      dtypes = "i32",
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
      dtypes = "ui32",
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
      dtypes = "pred",
      args = list(
        comparison_direction = direction,
        compare_type = "UNSIGNED"
      ),
      snapshot = FALSE
    )
  }
})

test_that("error tests", {
  local_func()
  lhs_f32 <- hlo_input("lhs_f32", "f32", shape = integer())
  rhs_f32 <- hlo_input("rhs_f32", "f32", shape = integer())
  lhs_i32 <- hlo_input("lhs_i32", "i32", shape = integer())
  rhs_i32 <- hlo_input("rhs_i32", "i32", shape = integer())
  lhs_ui32 <- hlo_input("lhs_ui32", "ui32", shape = integer())
  rhs_ui32 <- hlo_input("rhs_ui32", "ui32", shape = integer())
  lhs_pred <- hlo_input("lhs_pred", "pred", shape = integer())
  rhs_pred <- hlo_input("rhs_pred", "pred", shape = integer())

  # (I3) invalid comparison_direction
  expect_error(
    hlo_compare(
      lhs_f32,
      rhs_f32,
      comparison_direction = "INVALID",
      compare_type = "FLOAT"
    ),
    "comparison_direction"
  )

  # (I4) invalid compare_type
  expect_error(
    hlo_compare(
      lhs_f32,
      rhs_f32,
      comparison_direction = "LT",
      compare_type = "INVALID"
    ),
    "compare_type"
  )

  # (C3) FLOAT element type must have FLOAT or TOTALORDER compare_type
  expect_error(
    hlo_compare(
      lhs_f32,
      rhs_f32,
      comparison_direction = "LT",
      compare_type = "SIGNED"
    ),
    "FLOAT or TOTALORDER"
  )

  # (C3) SIGNED element type must have SIGNED compare_type
  expect_error(
    hlo_compare(
      lhs_i32,
      rhs_i32,
      comparison_direction = "LT",
      compare_type = "FLOAT"
    ),
    "must be SIGNED"
  )

  # (C3) UNSIGNED element type must have UNSIGNED compare_type
  expect_error(
    hlo_compare(
      lhs_ui32,
      rhs_ui32,
      comparison_direction = "LT",
      compare_type = "SIGNED"
    ),
    "must be UNSIGNED"
  )

  # (C3) Boolean (pred) element type must have UNSIGNED compare_type
  expect_error(
    hlo_compare(
      lhs_pred,
      rhs_pred,
      comparison_direction = "EQ",
      compare_type = "SIGNED"
    ),
    "must be UNSIGNED"
  )
})
