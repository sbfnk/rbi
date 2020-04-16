##' Get the LibBi version
##'
##' This tries to find the libbi executable to determine the installed version of LibBi
##' @param ... any arguments for \code{\link{locate_libbi}} (esp. \code{path_to_libbi})
##' @return a character string with the installed version
##' @author Sebastian Funk
##' @importFrom processx run
##' @keywords internal
##' @export
installed_libbi_version <- function(...) {
  path_to_libbi <- locate_libbi(...)
  cmd <- paste(path_to_libbi, "--version")
  version <-
      tryCatch(processx::run(path_to_libbi, args="--version")$stdout,
               error = function(e) NULL)
  if (is.null(version)) return("pre-1.4.3")
  version_string <- sub("LibBi version[[:space:]]*([0-9\\.A-z]+).*$", "\\1", version)
  return(version_string)
}
