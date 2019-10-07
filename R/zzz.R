.onLoad <- function(libname, pkgname) {
  stopifnot(requireNamespace("exams"))
  insert_imports_shims()
  invisible()
}

