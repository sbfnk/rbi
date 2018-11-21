##' Find the libbi executable
##'
##' This tries to find the libbi executable; if this does not find libbi but it is installed, the location can be either passed to this function, or set globally via `options(path_to_libbi="/insert/full/path/here").
##' @param path_to_libbi path to libbi, as either the path where the libbi executable resides, or the full path to the executable
##' @return full path to the libbi executable; if it is not found, an error is thrown
##' @author Sebastian Funk
##' @export
locate_libbi <- function(path_to_libbi) {
  if (missing(path_to_libbi) || length(path_to_libbi) == 0) {
    if (is.null(getOption("path_to_libbi"))) {
      ## Maybe the system knows where libbi is
      path <- unname(Sys.which("libbi"))
    } else {
      path <- getOption("path_to_libbi")
    }
    if (length(path) == 0){
      stop("Could not locate LibBi, please either provide the path to the libbi binary via the 'path_to_libbi' option, or set the PATH to contain the directory that contains the binary in ~/.Renviron or set it in your R session via options(path_to_libbi = \"insert_path_here\"). For instructions on how to install libbi, look at the RBi github page on https://github.com/libbi/rbi.")
    }
  } else {
    path <- path_to_libbi
  }
  if (!grepl("libbi$", path)) {
    path <- paste0(path, "/libbi")
  }
  if (!file.exists(path)) {
    stop("Could not find libbi executable at '", path, "'")
  }
  return(path)
}
