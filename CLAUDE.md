# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`stablehlo` is an R package that provides R bindings for StableHLO, a portable computation representation used in machine learning. It allows creating, manipulating, and transforming StableHLO operations in R.

## Development Commands

### Build and Install

```r
# Load the package for development
devtools::load_all()

# Install the package
devtools::install()

# Build the package (creates tar.gz file)
devtools::build()
```

### Testing

```r
# Run all tests
devtools::test()

# Run a specific test file
testthat::test_file("tests/testthat/test-constant.R")
```

### Documentation

```r
# Generate documentation from roxygen comments
devtools::document()
```

### Check

```r
# Run checks for CRAN compliance
devtools::check()
```

## Package Structure and Architecture

The package implements a representation of the StableHLO IR (Intermediate Representation) in R. The key components include:

1. **Types System** (`types.R`): Defines tensor types, element types, and shapes
   - `TensorType`, `TensorElementType`, `Shape`, `ValueType`
   - Various data types: `BooleanType`, `IntegerType`, `FloatType`, `ComplexType`

2. **Operations** (`op.R`, `ops.R`): Implements StableHLO operations
   - `OpMnemonic` - Enumeration of supported operations
   - `Op` - Base class for operations
   - `OpInputs`, `OpOutputs` - Handle operation inputs and outputs

3. **Functions** (`func.R`): Defines function representation
   - `Func`, `FuncBody`, `FuncInputs`, `FuncOutputs`

4. **Values and Constants** (`constant.R`, `value_id.R`):
   - `ValueId` - Identifier for values
   - `Constant` - Representation of constant values

5. **Shape Handling** (`shape.R`):
   - `Shape` - Representation of tensor shapes

6. **Utilities**:
   - `repr.R` - Representation helpers
   - `list_of.R` - Generic list type helpers
   - `enum.R` - Enumeration type support
   - `tracer.R` - Tracer for function execution

7. **JIT Compilation** (`package.R`):
   - `jit()` - Function to convert R functions to StableHLO code

## Development Practices

1. Use S7 (object-oriented system) for defining types and classes.
2. Follow the established pattern for adding new operations and types
3. Add tests in `tests/testthat/` with appropriate snapshots for validation
4. Document functions with roxygen2 comments
