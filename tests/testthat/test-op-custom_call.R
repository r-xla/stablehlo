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

test_that("backend_config carries array-valued attributes", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2, 3))
  out <- hlo_custom_call(
    x,
    call_target_name = "my_target",
    api_version = 4L,
    has_side_effect = FALSE,
    backend_config = CustomOpBackendConfig(list(
      StringAttr(name = "mode", value = "fast"),
      # renders as an MLIR dense array attribute, which the XLA FFI
      # decodes into `Span<const int64_t>`
      constant_attr(name = "axes", value = c(0L, 2L), dtype = "i64")
    )),
    output_types = list(ValueType("f32", shape = c(2, 3))),
    operand_layouts = list(c(1L, 0L)),
    result_layouts = list(c(1L, 0L))
  )
  f <- hlo_return(out)
  expect_snapshot(repr(f))
})

test_that("custom call with output_operand_aliases", {
  local_func()
  x <- hlo_input("x", "f32", shape = c(2, 3))
  out <- hlo_custom_call(
    x,
    call_target_name = "in_place",
    api_version = 4L,
    has_side_effect = FALSE,
    output_types = list(ValueType("f32", shape = c(2, 3))),
    operand_layouts = list(c(1L, 0L)),
    result_layouts = list(c(1L, 0L)),
    output_operand_aliases = list(OutputOperandAlias(operand_index = 0L))
  )
  f <- hlo_return(out)
  expect_snapshot(repr(f))
})

test_that("OutputOperandAlias renders tuple indices", {
  expect_snapshot(
    repr(OutputOperandAlias(
      operand_index = 1L,
      output_tuple_indices = 0L,
      operand_tuple_indices = 2L
    ))
  )
  expect_error(OutputOperandAlias(operand_index = -1L))
  expect_error(
    hlo_custom_call(
      call_target_name = "x",
      has_side_effect = FALSE,
      output_operand_aliases = list("nope")
    ),
    "May only contain the following types"
  )
})

test_that("CustomOpBackendConfig rejects unsupported attribute types", {
  expect_error(
    CustomOpBackendConfig(list(ValueType("f32", shape = 1))),
    "May only contain the following types"
  )
})

# No type inference errors (output types are user-specified)
