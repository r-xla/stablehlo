is_cuda <- function() {
  Sys.getenv("PJRT_PLATFORM", "cpu") == "cuda"
}

is_cpu <- function() {
  Sys.getenv("PJRT_PLATFORM", "cpu") == "cpu"
}
