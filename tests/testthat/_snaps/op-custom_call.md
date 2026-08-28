# print_tensor with header works on CPU

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>) -> tensor<2x2xf32> {\nstablehlo.custom_call @print_tensor(%x) {\n  call_target_name = \"print_tensor\",\n  api_version = 4 : i32,\n  has_side_effect = true,\n  backend_config = {\n    print_header = \"MyTensor\"\n  }\n} : (tensor<2x2xf32>) -> ()\nreturn %x : tensor<2x2xf32>\n}\n"

---

    Code
      out <- pjrt::pjrt_execute(exec, buf)
    Output
      MyTensor
       1 3
       2 4
      [ f32{2,2} ]

# custom call with operand and result layouts

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3xf32>) -> tensor<2x3xf32> {\n%0 = stablehlo.custom_call @my_target(%x) {\n  call_target_name = \"my_target\",\n  api_version = 4 : i32,\n  has_side_effect = false,\n  operand_layouts = [dense<[0, 1]> : tensor<2xindex>],\n  result_layouts = [dense<[1, 0]> : tensor<2xindex>]\n} : (tensor<2x3xf32>) -> (tensor<2x3xf32>)\nreturn %0 : tensor<2x3xf32>\n}\n"

# backend_config carries array-valued attributes

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3xf32>) -> tensor<2x3xf32> {\n%0 = stablehlo.custom_call @my_target(%x) {\n  call_target_name = \"my_target\",\n  api_version = 4 : i32,\n  has_side_effect = false,\n  backend_config = {\n    mode = \"fast\",\n    axes = array<i64: 0, 2>\n  },\n  operand_layouts = [dense<[1, 0]> : tensor<2xindex>],\n  result_layouts = [dense<[1, 0]> : tensor<2xindex>]\n} : (tensor<2x3xf32>) -> (tensor<2x3xf32>)\nreturn %0 : tensor<2x3xf32>\n}\n"

# custom call with output_operand_aliases

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x3xf32>) -> tensor<2x3xf32> {\n%0 = stablehlo.custom_call @in_place(%x) {\n  call_target_name = \"in_place\",\n  api_version = 4 : i32,\n  has_side_effect = false,\n  operand_layouts = [dense<[1, 0]> : tensor<2xindex>],\n  result_layouts = [dense<[1, 0]> : tensor<2xindex>],\n  output_operand_aliases = [#stablehlo.output_operand_alias<output_tuple_indices = [], operand_index = 0, operand_tuple_indices = []>]\n} : (tensor<2x3xf32>) -> (tensor<2x3xf32>)\nreturn %0 : tensor<2x3xf32>\n}\n"

# OutputOperandAlias renders tuple indices

    Code
      repr(OutputOperandAlias(operand_index = 1L, output_tuple_indices = 0L,
        operand_tuple_indices = 2L))
    Output
      [1] "#stablehlo.output_operand_alias<output_tuple_indices = [0], operand_index = 1, operand_tuple_indices = [2]>"

