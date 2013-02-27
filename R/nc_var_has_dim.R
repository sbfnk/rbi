# Copyright (C) 2011-2013-2013
# Author: Lawrence Murray <lawrence.murray@csiro.au>
#$Rev: 1687 $
#$Date: 2011-06-28 11:46:45 +0800 (Tue, 28 Jun 2011) $
#-*- texinfo -*-
#@deftypefn {Function File} {@var{check} = } nc_var_has_dim (@var{nc}, @var{name}, @var{dim})
#
# Does NetCDF variable have a particular dimension?
#
# @itemize
# @item @var{nc} NetCDF file handle.
#
# @item @var{name} Name of the variable.
#
# @item @var{dim} Name of the dimension.
#
# @end itemize
# @end deftypefn
#
nc_var_has_dim <- function(nc, name, dim){
  # check arguments
  # if nargin != 3
  # print_usage ();
  # end
  dimnames <- nc_var_get_dim(nc, name)
  return(dim %in% dimnames)
}

