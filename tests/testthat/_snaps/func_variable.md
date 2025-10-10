# c works

    Code
      z[[1]]@func
    Output
      func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
      
      }

---

    Code
      z[[2]]@func
    Output
      func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
      
      }

---

    Code
      f
    Output
      func.func @main (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) -> (tensor<2x2xf32>, tensor<2x2xf32>) {
      %0 = "stablehlo.add" (%x, %y): (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
      %1 = "stablehlo.add" (%x, %y): (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
      "func.return"(%0, %1): (tensor<2x2xf32>, tensor<2x2xf32>) -> ()
      }

# repr

    Code
      x
    Output
      Variable %x in:
      func.func @main (%x: tensor<2x2xf32>) ->  {
      
      }

