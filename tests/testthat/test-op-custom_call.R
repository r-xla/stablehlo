test_that("print_tensor with header works on CPU", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2, 2))
  hlo_custom_call(
    x,
    call_target_name = "print_tensor",
    api_version = 4L,
    has_side_effect = TRUE,
    backend_config = CustomOpBackendConfig(list(
      StringAttr(name = "print_header", value = "MyTensor")
    ))
  )
  f <- hlo_return(x)
  expect_snapshot(repr(f))

  skip_if_not_installed("pjrt")
  # printing currently only works on CPU
  skip_if(!is_cpu())

  program <- pjrt::pjrt_program(repr(f))
  exec <- pjrt::pjrt_compile(program)

  buf <- pjrt::pjrt_buffer(matrix(1:4, nrow = 2, ncol = 2), dtype = "f32")

  expect_snapshot({
    out <- pjrt::pjrt_execute(exec, buf)
  })

  expect_equal(out, buf)
})

test_that("custom call with operand and result layouts", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2, 3))
  out <- hlo_custom_call(
    x,
    call_target_name = "my_target",
    api_version = 4L,
    has_side_effect = FALSE,
    output_types = list(ValueType("f32", shape = c(2, 3))),
    operand_layouts = list(c(0L, 1L)),
    result_layouts = list(c(1L, 0L))
  )
  f <- hlo_return(out)
  expect_snapshot(repr(f))
})

# No type inference errors (output types are user-specified)

test_that("a target name that is not a bare identifier is quoted", {
  # MLIR symbol references are bare identifiers unless quoted; upstream prints
  # them with `printSymbolName`. An FFI target with a `-` in it emitted text
  # that did not parse.
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  hlo_custom_call(
    x,
    call_target_name = "foo-bar",
    has_side_effect = FALSE,
    output_types = list(ValueType("f32", shape = c(2L, 2L)))
  )
  expect_match(repr(.current_func()), '@"foo-bar"', fixed = TRUE)
})

test_that("api_version is one of the five the ODS defines", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  expect_snapshot(
    hlo_custom_call(
      x,
      call_target_name = "foo",
      api_version = 99L,
      has_side_effect = FALSE,
      output_types = list(ValueType("f32", shape = c(2L, 2L)))
    ),
    error = TRUE
  )
})

test_that("a dictionary backend_config requires the typed-FFI api_version", {
  # `API_VERSION_TYPED_FFI` is 4; every other version wants a string config.
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 2L))
  expect_snapshot(
    hlo_custom_call(
      x,
      call_target_name = "foo",
      api_version = 1L,
      has_side_effect = FALSE,
      backend_config = CustomOpBackendConfig(list(
        StringAttr(name = "k", value = "v")
      )),
      output_types = list(ValueType("f32", shape = c(2L, 2L)))
    ),
    error = TRUE
  )
})

test_that("layouts are all-or-nothing, counted, and permutations", {
  cc <- function(...) {
    local_func()
    x <- hlo_input("x", "f32", shape = c(2L, 3L))
    hlo_custom_call(
      x,
      call_target_name = "foo",
      has_side_effect = FALSE,
      output_types = list(ValueType("f32", shape = c(2L, 3L))),
      ...
    )
  }
  # The verifier wants both or neither.
  expect_snapshot(cc(operand_layouts = list(c(1L, 0L))), error = TRUE)
  # One entry per operand and per result.
  expect_snapshot(
    cc(
      operand_layouts = list(c(1L, 0L), c(1L, 0L)),
      result_layouts = list(c(1L, 0L))
    ),
    error = TRUE
  )
  # Each entry is a permutation of that value's axes.
  expect_snapshot(
    cc(operand_layouts = list(0L), result_layouts = list(c(1L, 0L))),
    error = TRUE
  )
  # A valid pair is accepted.
  expect_no_error(
    cc(operand_layouts = list(c(1L, 0L)), result_layouts = list(c(1L, 0L)))
  )
})
