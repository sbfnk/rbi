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
#   if (nargin < 2 || nargin > 5){
#     print_usage ();
#   }
#   if (!ischar (name)){
#   error ('name must be a string');
#   }
# if nargin < 3
# coord = [];
# end
# if nargin < 4
# ps = [];
# end
# if nargin < 5
# ts = [];
# end
  
  # defer to implementation for file schema
#   if (nc_var_has_dim (nc, name, 'nrp')){
#     f = @read_var_flexi_simulator;
#   } else {
  ############ How to get the global attributes in R? It doesn't seem to be in 
  ############ the ncdf package -> use the C interface  
#     if (isempty (nc.libbi_schema))
#       f = @read_var_input;
#     } else {
#       f = @read_var_simulator;
#     }
#   }
#   X = f (nc, name, coord, ps, ts);
#   return(X)
}