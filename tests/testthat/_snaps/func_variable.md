# c works

    Code
      z$x@func
    Output
      func.func @ (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
      
      }

---

    Code
      z$y@func
    Output
      func.func @ (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) ->  {
      
      }

---

    Code
      f
    Output
      func.func @ (%x: tensor<2x2xf32>, %y: tensor<2x2xf32>) -> tensor<2x2xf32>, tensor<2x2xf32> {
      %1 = "stablehlo.add" (%x, %y): (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
      %2 = "stablehlo.add" (%x, %y): (tensor<2x2xf32>, tensor<2x2xf32>) -> (tensor<2x2xf32>)
      "func.return"(%1, %2): (tensor<2x2xf32>, tensor<2x2xf32>) -> ()
      }

# repr

    Code
      x
    Output
      Variable %x in:
      func.func @main (%x: tensor<2x2xf32>) ->  {
      
      }

