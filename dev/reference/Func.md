# Func

This represents a function. Note: Func uses reference semantics -
modifications to a Func object modify the original.

## Usage

``` r
Func(
  id = FuncId(),
  inputs = FuncInputs(),
  outputs = FuncOutputs(),
  body = FuncBody()
)
```

## Arguments

- id:

  (`FuncId`  
  The id of the function.

- inputs:

  (`FuncInputs`  
  The inputs of the function.

- outputs:

  (`FuncOutputs`  
  The outputs of the function.

- body:

  (`FuncBody`  
  The body of the function.

## Value

A `Func` object.
