# custom_call generates correct representation

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<4xf32>) -> tensor<4xf32> {\nstablehlo.custom_call @print_tensor(%x) {\n  call_target_name = \"print_tensor\",\n  has_side_effect = true,\n  api_version = 4 : i32\n} : (tensor<4xf32>) -> ()\n\"func.return\"(%x): (tensor<4xf32>) -> ()\n}\n"

# custom_call with backend_config

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<4xf32>) -> tensor<4xf32> {\nstablehlo.custom_call @print_tensor(%x) {\n  call_target_name = \"print_tensor\",\n  has_side_effect = true,\n  api_version = 4 : i32,\n  backend_config = {\n    print_header = \"TestHeader\"\n  }\n} : (tensor<4xf32>) -> ()\n\"func.return\"(%x): (tensor<4xf32>) -> ()\n}\n"

# custom_call with multiple inputs

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<4xf32>, %y: tensor<4xi32>) -> tensor<4xf32> {\nstablehlo.custom_call @multi_input_handler(%x, %y) {\n  call_target_name = \"multi_input_handler\",\n  has_side_effect = true,\n  api_version = 4 : i32\n} : (tensor<4xf32>, tensor<4xi32>) -> ()\n\"func.return\"(%x): (tensor<4xf32>) -> ()\n}\n"

# print_tensor with header works on CPU

    Code
      out <- pjrt::pjrt_execute(exec, buf)
    Output
      MyTensor
       1.0000 3.0000
       2.0000 4.0000
      [ F32{2,2} ]

# print_tensor with integer tensor

    Code
      out <- pjrt::pjrt_execute(exec, buf)
    Output
      PJRTBuffer
       10
       20
       30
       40
      [ S32{4} ]

# print_tensor with boolean tensor

    Code
      out <- pjrt::pjrt_execute(exec, buf)
    Output
      PJRTBuffer
       1
       0
       1
       0
      [ PRED{4} ]

