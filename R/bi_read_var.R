#' @rdname bi_read_var
#' @name bi_read_var
#' @title Bi Read Variable
#' @description
#' This function reads a variable from a NetCDF file.
#' The file can be specified as a string to the filepath, in which
#' case a NetCDF connection is opened, or directly as a NetCDF connection.
#' 
#' @param resultfile either a path to a NetCDF file, or a NetCDF connection created using open.ncdf(filename)
#' @param name name of the variable to read (use \code{\link{bi_file_summary}} to learn about the variable names of a specific file)
#' @param coord dimension indices (not implemented yet)
#' @param ts time indices (not implemented yet)
#' @export
#
bi_read_var <- function(resultfile, name, coord, ps, ts){
  # check arguments
  # if the result file is provided as a string representing the path
  # then open a ncdf connection
  if (typeof(resultfile) == "character"){
    nc <- open.ncdf(tools::file_path_as_absolute(resultfile))
  } else {
    nc <- resultfile
  }
  if (!is.character (name)){
    stop('name must be a string');
  }
  if (missing(coord)){
    coord = c()
  }
  if (missing(ps)){
    ps = c()
  }
  if (missing(ts)){
    ts = c()
  }
  global_attributes = nc_get_attributes(nc)
  if (nc_var_has_dim(nc, name, "nrp")){
    myfunction = read_var_flexi_simulator
  } else {
    if ("libbi_schema" %in% names(global_attributes)){
      if (length(global_attributes[["libbi_schema"]]) == 0){
        myfunction = read_var_input
      } else {
        myfunction = read_var_simulator
      }
    } else {
      myfunction = read_var_input
    }
  }
  X = myfunction(nc, name, coord, ps, ts)
  if (typeof(resultfile) == "character"){
    close.ncdf(nc)
  }
  return(X)
}