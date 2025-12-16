is_cuda <- function() {
  Sys.getenv("PJRT_PLATFORM") == "cuda"
}
