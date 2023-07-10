#' @rdname bi_file_summary
#' @name bi_file_summary
#' @title NetCDF File Summary
#' @description
#' This function prints a little summary of the content
#' of a NetCDF file, as well as its creation time. You can
#' then retrieve variables of interest using \code{\link{bi_read}}.
#' @param ... Any extra parameters to \code{\link{bi_open}}, especially \code{x}
#'   and \code{file}
#' @return No return value
#' @export
#' @importFrom ncdf4 nc_close
#' @examples
#' example_output_file <- system.file(package = "rbi", "example_output.nc")
#' bi_file_summary(example_output_file)
bi_file_summary <- function(...) {
  ncfile <- bi_open(...)
  print(ncfile)
  nc_close(ncfile)
}
