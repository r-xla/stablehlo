# print_tensor with header works on CPU

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>) -> tensor<2x2xf32> {\nstablehlo.custom_call @print_tensor(%x) {\n  call_target_name = \"print_tensor\",\n  api_version = 4 : i32,\n  has_side_effect = true,\n  backend_config = {\n    print_header = \"MyTensor\"\n  }\n} : (tensor<2x2xf32>) -> ()\n\"func.return\"(%x): (tensor<2x2xf32>) -> ()\n}\n"

---

    Code
      out <- pjrt::pjrt_execute(exec, buf)
    Output
      MyTensor
       1 3
       2 4
      [ F32{2,2} ]

