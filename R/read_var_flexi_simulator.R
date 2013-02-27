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
read_var_flexi_simulator <- function(nc, name, coord, ps, ts){
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
  } else {
    stop("you specified particle indices but in this file the number of particles varies across time")
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
  len = get.var.ncdf(ncfile2, "len")
  start = get.var.ncdf(ncfile2, "start")
  
  variable = list()
  counter = 0
  for (i in 1:length(start)){
    if (i %in% ts){
      counter = counter + 1
      from = start[i] + 1
      to = (start[i] + 1) + len[i]
      variable_t = values[from:to]
      variable[[counter]] = variable_t
    }
  }
  return(variable)
  # args = {};
  # if nc_var_has_dim (nc, name, 'nr')
  # args{length (args) + 1} = ts;
  # end
  # if length (coord) > 0
  # from = length (args) + 1;
  # to = from + length (coord) - 1;
  # args{from:to} = num2cell (coord){:};
  # end
  # if nc_var_has_dim (nc, name, 'np')
  # args{length (args) + 1} = ps;
  # end
  # X = full_squeeze(nc{name}(args{:}));
  # end
}
