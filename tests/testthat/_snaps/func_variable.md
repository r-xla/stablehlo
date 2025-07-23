# c works

    Code
      z$x
    Output
      <stablehlo::FuncVariable>
       @ value_id  : <stablehlo::ValueId> function (id)  
       .. @ id: chr "x"
       @ value_type: <stablehlo::ValueType> function (type, shape = NULL)  
       .. @ type: <stablehlo::TensorType>
       .. .. @ dtype: <stablehlo::TensorElementType>
       .. .. .. @ type: <stablehlo::FloatType>
       .. .. .. .. @ Value   : chr "f32"
       .. .. .. .. @ Variants: chr [1:15] "f4E2M1FN" "f6E2M3FN" "f6E3M2FN" ...
       .. .. @ shape: <stablehlo::Shape>
       .. .. .. @ dims: num [1:2] 2 2
       @ func      : <stablehlo::Func>
       .. @ id     : <stablehlo::FuncId>
       .. .. @ id: chr(0) 
       .. @ inputs : <stablehlo::FuncInputs>
       .. .. @ items:List of 2
       .. .. .. $ : <stablehlo::FuncInput>
       .. .. ..  ..@ id  : <stablehlo::ValueId> function (id)  
       .. .. .. .. .. @ id: chr "x"
       .. .. ..  ..@ type: <stablehlo::ValueType> function (type, shape = NULL)  
       .. .. .. .. .. @ type: <stablehlo::TensorType>
       .. .. .. .. .. .. @ dtype: <stablehlo::TensorElementType>
       .. .. .. .. .. .. .. @ type: <stablehlo::FloatType>
       .. .. .. .. .. .. .. .. @ Value   : chr "f32"
       .. .. .. .. .. .. .. .. @ Variants: chr [1:15] "f4E2M1FN" ...
       .. .. .. .. .. .. @ shape: <stablehlo::Shape>
       .. .. .. .. .. .. .. @ dims: num [1:2] 2 2
       .. .. .. $ : <stablehlo::FuncInput>
       .. .. ..  ..@ id  : <stablehlo::ValueId> function (id)  
       .. .. .. .. .. @ id: chr "y"
       .. .. ..  ..@ type: <stablehlo::ValueType> function (type, shape = NULL)  
       .. .. .. .. .. @ type: <stablehlo::TensorType>
       .. .. .. .. .. .. @ dtype: <stablehlo::TensorElementType>
       .. .. .. .. .. .. .. @ type: <stablehlo::FloatType>
       .. .. .. .. .. .. .. .. @ Value   : chr "f32"
       .. .. .. .. .. .. .. .. @ Variants: chr [1:15] "f4E2M1FN" ...
       .. .. .. .. .. .. @ shape: <stablehlo::Shape>
       .. .. .. .. .. .. .. @ dims: num [1:2] 2 2
       .. @ outputs: <stablehlo::FuncOutputs>
       .. .. @ items: list()
       .. @ body   : <stablehlo::FuncBody>
       .. .. @ items: list()

---

    Code
      z$y
    Output
      <stablehlo::FuncVariable>
       @ value_id  : <stablehlo::ValueId> function (id)  
       .. @ id: chr "y"
       @ value_type: <stablehlo::ValueType> function (type, shape = NULL)  
       .. @ type: <stablehlo::TensorType>
       .. .. @ dtype: <stablehlo::TensorElementType>
       .. .. .. @ type: <stablehlo::FloatType>
       .. .. .. .. @ Value   : chr "f32"
       .. .. .. .. @ Variants: chr [1:15] "f4E2M1FN" "f6E2M3FN" "f6E3M2FN" ...
       .. .. @ shape: <stablehlo::Shape>
       .. .. .. @ dims: num [1:2] 2 2
       @ func      : <stablehlo::Func>
       .. @ id     : <stablehlo::FuncId>
       .. .. @ id: chr(0) 
       .. @ inputs : <stablehlo::FuncInputs>
       .. .. @ items:List of 2
       .. .. .. $ : <stablehlo::FuncInput>
       .. .. ..  ..@ id  : <stablehlo::ValueId> function (id)  
       .. .. .. .. .. @ id: chr "x"
       .. .. ..  ..@ type: <stablehlo::ValueType> function (type, shape = NULL)  
       .. .. .. .. .. @ type: <stablehlo::TensorType>
       .. .. .. .. .. .. @ dtype: <stablehlo::TensorElementType>
       .. .. .. .. .. .. .. @ type: <stablehlo::FloatType>
       .. .. .. .. .. .. .. .. @ Value   : chr "f32"
       .. .. .. .. .. .. .. .. @ Variants: chr [1:15] "f4E2M1FN" ...
       .. .. .. .. .. .. @ shape: <stablehlo::Shape>
       .. .. .. .. .. .. .. @ dims: num [1:2] 2 2
       .. .. .. $ : <stablehlo::FuncInput>
       .. .. ..  ..@ id  : <stablehlo::ValueId> function (id)  
       .. .. .. .. .. @ id: chr "y"
       .. .. ..  ..@ type: <stablehlo::ValueType> function (type, shape = NULL)  
       .. .. .. .. .. @ type: <stablehlo::TensorType>
       .. .. .. .. .. .. @ dtype: <stablehlo::TensorElementType>
       .. .. .. .. .. .. .. @ type: <stablehlo::FloatType>
       .. .. .. .. .. .. .. .. @ Value   : chr "f32"
       .. .. .. .. .. .. .. .. @ Variants: chr [1:15] "f4E2M1FN" ...
       .. .. .. .. .. .. @ shape: <stablehlo::Shape>
       .. .. .. .. .. .. .. @ dims: num [1:2] 2 2
       .. @ outputs: <stablehlo::FuncOutputs>
       .. .. @ items: list()
       .. @ body   : <stablehlo::FuncBody>
       .. .. @ items: list()

