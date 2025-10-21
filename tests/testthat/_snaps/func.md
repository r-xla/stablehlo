# Func repr

    Code
      repr(func)
    Output
      [1] "func.func @my_func (%x: tensor<1x2xf32>, %y: tensor<1x2xf32>) -> tensor<1x2xf32> {\n\n}\n"

# multiple returns

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32>) -> (tensor<2x2xf32>, tensor<2x2xf32>) {\n\"func.return\"(%x, %x): (tensor<2x2xf32>, tensor<2x2xf32>) -> ()\n}\n"

# Input-output aliasing

    Code
      repr(f)
    Output
      [1] "func.func @main (%x: tensor<2x2xf32> {tf.aliasing_output = 0 : i32}) -> tensor<2x2xf32> {\n\"func.return\"(%x): (tensor<2x2xf32>) -> ()\n}\n"

---

    Code
      print(xout)
    Output
      PJRTBuffer 
       1.0000 3.0000
       2.0000 4.0000
      [ CPUf32{2x2} ] 

