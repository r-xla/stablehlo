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

test_that("OutputOperandAlias names the aliased result", {
  expect_snapshot(
    repr(OutputOperandAlias(operand_index = 1L, output_tuple_indices = 0L))
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

test_that("inference declares every custom attribute the builder passes", {
  # `hlo_fn()` hands custom attributes to the inference function *by name*, so
  # a name the inference function does not declare is swallowed by its `...`
  # and counted as an operand. That is silent: it does not error, it makes
  # `operands` one longer, which threw the layout checks off by one and broke
  # every call passing layouts -- including ones passing the new attribute as
  # `NULL`. Pinning the two signatures against each other catches the next
  # attribute added to the builder without a matching argument here.
  builder <- names(formals(hlo_custom_call))
  inference <- names(formals(infer_types_custom_call))
  # `...` is the operands, and `call_target_name`/`api_version`/
  # `has_side_effect` arrive as `attrs` rather than `custom_attrs`; every
  # other builder argument is a custom attribute.
  custom_attrs <- setdiff(builder, "...")
  expect_setequal(intersect(custom_attrs, inference), custom_attrs)
})

test_that("a multi-result call names the aliased result", {
  # StableHLO's verifier builds a tuple out of the result types whenever a
  # custom call has more than one result, so an alias on a multi-result call
  # must say *which* result it means -- with an empty `output_tuple_indices`
  # the operand is compared against the whole synthesised tuple and the
  # program is rejected. anvl's `nv_custom_call()` relies on this.
  local_func()
  x <- hlo_input("x", "f32", shape = c(2L, 3L))
  out <- hlo_custom_call(
    x,
    call_target_name = "t",
    has_side_effect = FALSE,
    output_types = list(
      ValueType("f32", shape = c(2L, 3L)),
      ValueType("f32", shape = 2L)
    ),
    output_operand_aliases = list(
      OutputOperandAlias(operand_index = 0L, output_tuple_indices = 0L)
    )
  )
  src <- repr(hlo_return(out[[1L]]))
  expect_match(src, "output_tuple_indices = [0]", fixed = TRUE)
  # The operand path is always empty here -- there are no tuple-typed operands
  # to walk into -- but the attribute's syntax still requires the field.
  expect_match(src, "operand_tuple_indices = []", fixed = TRUE)
})
