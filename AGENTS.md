## Package Overview

`stablehlo` is an R package that allows to create StableHLO programs, a portable computation representation used in machine learning. It allows creating, manipulating, and transforming StableHLO operations in R.
The Func object uses reference semantic, while other objects use value semantics.

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

You can compare PJRTBuffers using `expect_equal()`, so you don't need to use `as_array()`.

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

## Development Practices

1. Use S3 (object-oriented system) for defining types and classes.
2. Follow the established pattern for adding new operations and types.
3. Add tests in `tests/testthat/` with appropriate snapshots and execution tests for validation.
4. Document functions with roxygen2 comments.
5. Run `make format` to format the code.

## Adding New Operations

When implementing a new operation, closely follow the specification described in SPEC.md.
Also, annotate each check in the inference function with the corresponding requirement from the specification (C1, C2, C3, etc.).

## Error Messages

The error messages should make it clear what went wrong (i.e. what was expected) and what the actual input was.
Also, error messages referencing 0-based values (like indices) should be wrapped in custom error classes (reusing existing error classes if possible) and implement the `to_one_based()` generic function to convert the 0-based values to 1-based values in the error message.

## Linter

To check for linter errors, you need to install the package first using `devtools::install()`.
Then, run `lintr::lint_package()`.
For linter errors where the line is too long, but the line is a string, put `#fmt: skip` before the line and append `# nolint` at the end of the line.
