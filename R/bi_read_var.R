# Copyright (C) 2011-2013-2013
# Author: Lawrence Murray <lawrence.murray@csiro.au>
#  % $Rev: 3064 $
#  % $Date: 2012-09-09 15:01:02 +0800 (Sun, 09 Sep 2012) $
#  
#  % -*- texinfo -*-
#  % @deftypefn {Function File} {@var{X} = } bi_read_var (@var{nc}, @var{name}, @var{coord}, @var{ps}, @var{ts})
#
# Read variable from NetCDF file.
#
# @itemize
# @item @var{nc} NetCDF file.
#
# @item @var{name} Name of the variable.
#
# @item @var{coord} (optional) Dimension indices.
#
# @item @var{ps} (optional) Path indices.
#
# @item @var{ts} (optional) Time indices.
# @end itemize
# @end deftypefn
#
bi_read_var <- function(nc, name, coord, ps, ts){
  # check arguments
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
    }
  }
  X = myfunction(nc, name, coord, ps, ts)
  return(X)
}