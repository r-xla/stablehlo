globals <- new.env()
globals[["FUNC_STASH"]] <- list()
globals[["CURRENT_FUNC"]] <- NULL
globals[["dtypes"]] <- c(
  "pred",
  "i8",
  "i16",
  "i32",
  "i64",
  "ui8",
  "ui16",
  "ui32",
  "ui64",
  "f32",
  "f64"
)
globals[["infer_fn"]] <- hashtab()
