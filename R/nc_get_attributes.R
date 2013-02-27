# Copyright (C) 2011-2012-2013
# Author: 
#-*- texinfo -*-
# Get all the global attributes
#
# @itemize
# @item @var{nc} NetCDF file handle.
#
# @end itemize
#
nc_get_attributes <- function(nc){
  # check arguments
  # if nargin != 3
  # print_usage ();
  # end
  attributes = nc_get_attributes_from_path(nc$filename)
  refactorisedattributes = list()
  for (j in 1:length(aa$names)){
    refactorisedattributes[attributes$names[j]] = attributes$values[j]
  }
  return(refactorisedattributes)
}
