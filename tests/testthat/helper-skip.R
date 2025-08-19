skip_if_metal <- function(msg = "") {
  if (is_metal()) {
    testthat::skip(sprintf("Skipping test on Metal device: %s", msg))
  }
}

is_metal <- function() {
  Sys.getenv("PJRT_PLATFORM") == "metal"
}

is_cuda <- function() {
  Sys.getenv("PJRT_PLATFORM") == "cuda"
}
