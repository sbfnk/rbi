# Copyright (C) 2011-2012-2013
# Author: Lawrence Murray <lawrence.murray@csiro.au>
#-*- texinfo -*-
#@deftypefn {Function File} {@var{check} = } nc_var_has_dim (@var{nc}, @var{name}, @var{dim})
#
# Get all the dimension names of a variable in a NetCDF file
#
# @itemize
# @item @var{nc} NetCDF file handle.
#
# @item @var{name} Name of the variable.
#
# @end itemize
# @end deftypefn
#
nc_var_get_dim <- function(nc, name){
  # check arguments
  # if nargin != 3
  # print_usage ();
  # end
  dims <- nc$var[[name]]$dim
  dimnames <- c()
  for (i in 1:(length(dims))){
    dimnames <- c(dimnames, dims[[i]]$name)
  }
  return(dimnames)
}
