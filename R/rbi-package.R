## set global option
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("libbi_args"))) options(libbi_args = list())
}
