##' Read variable from NetCDF file.
##'
##' @param nc NetCDF file handle
##' @param name Name of the variable
##' @param coord (optional) Demsions index.
##' @param ps (optional) Path indices.
##' @param ts (optional) Time indices.
##' @param ... options for ncvar_get
##' @return read values
##' @author Lawrence Murray, \email{lawrence.murray@@csiro.au}
##' @importFrom ncdf4 ncvar_get
read_var_input <- function(nc, name, coord, ps, ts, ...){
  # check arguments
  # if nargin < 2 || nargin > 5
  # print_usage ();
  # end
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
  # check dimensions
#   if (nc_var_has_dim (nc, name, 'np')){
#     P = length(nc$dim[["np"]]$vals)
#   } else {
#     P = 1
#   }
#   if (length(ps) == 0){
#     ps = 1:P
#   }
#   if (nc_var_has_dim (nc, name, 'nr')){
#     T = length(nc$dim[["nr"]]$vals);
#   } else {
#     T = 1
#   }
#   if (length(ts) == 0){
#     ts = 1:T
#   }
  # read
  values = ncvar_get(nc, name, ...)
  if (is.null(dim(values))){
    return(values)
  } else {
    values_permutated <- aperm(a=values, perm=rev(1:length(dim(values))))  
    return(values_permutated)
  }
}
