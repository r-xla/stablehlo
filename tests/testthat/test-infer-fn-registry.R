test_that("all implemented ops have infer_fn registered in globals", {
  # This test ensures that all ops have their type inference functions registered
  # in globals[["infer_fn"]]. Most ops use hlo_fn() which automatically registers
  # the infer function, but some (like constant and iota) manually register it.
  # This test catches any ops that were implemented but forgot to register.

  # Find all op files by looking in the package directory
  pkg_path <- system.file(package = "stablehlo")

  # If pkg_path is empty, we're probably in dev mode, use relative path
  if (pkg_path == "") {
    r_dir <- file.path(getwd(), "..", "..", "R")
  } else {
    r_dir <- file.path(pkg_path, "..", "R")
  }

  # Normalize the path
  r_dir <- normalizePath(r_dir, mustWork = FALSE)

  # Check that the R directory exists
  if (!dir.exists(r_dir)) {
    skip(sprintf("Could not find R directory at: %s", r_dir))
  }

  # Find all op files
  op_files <- list.files(
    path = r_dir,
    pattern = "^op-.*\\.R$",
    full.names = FALSE
  )

  # Extract op names from filenames
  # e.g., "op-abs.R" -> "abs"
  op_names <- gsub("^op-(.*)\\.R$", "\\1", op_files)

  # Check that each op has an infer_fn registered
  missing_infer_fns <- character(0)

  for (op_name in op_names) {
    infer_fn <- globals[["infer_fn"]][[op_name]]
    if (is.null(infer_fn)) {
      missing_infer_fns <- c(missing_infer_fns, op_name)
    }
  }

  # Report any missing infer_fns
  if (length(missing_infer_fns) > 0) {
    msg <- sprintf(
      "The following ops are missing infer_fn in globals: %s",
      paste(missing_infer_fns, collapse = ", ")
    )
    fail(msg)
  }

  # If we get here, all ops have infer_fn registered
  expect_true(length(op_names) > 0, info = "Should have at least one op")
  expect_true(
    length(missing_infer_fns) == 0,
    info = "All ops should have infer_fn registered"
  )
})
