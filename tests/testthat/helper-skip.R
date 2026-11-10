is_cuda <- function() {
  Sys.getenv("PJRT_PLATFORM", "cpu") == "cuda"
}

is_cpu <- function() {
  Sys.getenv("PJRT_PLATFORM", "cpu") == "cpu"
}

# Compiling and executing programs requires a PJRT plugin, which is downloaded
# over the network. That is not possible on CRAN, so such tests only run when
# the plugin is already available or may be downloaded (`PJRT_INSTALL=1`, as
# set on CI).
skip_if_no_pjrt <- function() {
  testthat::skip_if_not_installed("pjrt")
  platform <- Sys.getenv("PJRT_PLATFORM", "cpu")
  available <- pjrt::plugins_downloaded(platform) ||
    Sys.getenv("PJRT_INSTALL") == "1" ||
    nzchar(Sys.getenv(paste0("PJRT_PLUGIN_PATH_", toupper(platform))))
  if (!available) {
    testthat::skip("PJRT plugin is not available.")
  }
}
