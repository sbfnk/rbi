##' Read variable from NetCDF file.
##'
##' @param nc NetCDF file handle
##' @param name Name of the variable
##' @param coord (optional) Dimension index.
##' @param ps (optional) Path indices.
##' @param ts (optional) Time indices.
##' @param ... options for ncvar_get
##' @return read values
##' @author Lawrence Murray, \email{lawrence.murray@@csiro.au}
##' @importFrom ncdf4 ncvar_get
read_var_input <- function(nc, name, coord, ps, ts, ...){
  if (!is.character (name)){
    stop('name must be a string')
  }
  if (missing(coord)){
    coord <- c()
  }
  if (missing(ps)){
    ps <- c()
  }
  if (missing(ts)){
    ts <- c()
  }
  # read
  values <- ncvar_get(nc, name, ...)
  if (is.null(dim(values))){
    return(values)
  } else {
    values_permutated <- aperm(a=values, perm=rev(seq_along(dim(values))))  
    return(values_permutated)
  }
}
