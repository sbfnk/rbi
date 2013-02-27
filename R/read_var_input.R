# Copyright (C) 2011-2013-2013
# Author: Lawrence Murray <lawrence.murray@csiro.au>
#   % $Rev: 3551 $
#   % $Date: 2013-02-26 11:10:42 +0800 (Tue, 26 Feb 2013) $
#   
#   % -*- texinfo -*-
#   % @deftypefn {Function File} {@var{X} = } read_var_simulator (@var{nc}, @var{name}, @var{coord}, @var{ps}, @var{ts})
#
# Read variable from NetCDF file.
#
# @itemize
# @item @var{nc} NetCDF file handle.
#
# @item @var{name} Name of the variable.
#
# @item @var{coord} (optional) Dimensions index.
#
# @item @var{ps} (optional) Path indices.
#
# @item @var{ts} (optional) Time indices.
#
# @end itemize
# @end deftypefn
#
read_var_input <- function(nc, name, coord, ps, ts){
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
  if (nc_var_has_dim (nc, name, 'np')){
    P = length(nc$dim[["np"]]$vals)
  } else {
    P = 1
  }
  if (length(ps) == 0){
    ps = 1:P
  }
  if (nc_var_has_dim (nc, name, 'nr')){
    T = length(nc$dim[["nr"]]$vals);
  } else {
    T = 1
  }
  if (length(ts) == 0){
    ts = 1:T
  }
  # read
  values = get.var.ncdf(nc, name)
  return(values)
}
