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

1. Use S7 (object-oriented system) for defining types and classes.
2. Follow the established pattern for adding new operations and types.
3. Add tests in `tests/testthat/` with appropriate snapshots and execution tests for validation.
4. Document functions with roxygen2 comments.
