nc_get_attributes <- function(nc){
  # check arguments
  # if nargin != 3
  # print_usage ();
  # end
  attributes = nc_get_attributes_from_path(nc$filename)
  refactorisedattributes = list()
  for (j in 1:length(attributes$names)){
    refactorisedattributes[attributes$names[j]] = attributes$values[j]
  }
  return(refactorisedattributes)
}
