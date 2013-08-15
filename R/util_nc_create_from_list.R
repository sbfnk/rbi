#' @rdname netcdf_create_from_list
#' @name netcdf_create_from_list
#' @aliases netcdf_create_from_list
#' @title Create NetCDF File from R list
#' @description
#' This function creates a NetCDF file given a list.
#' @param filename a path to a NetCDF file to write the variable into, which will be overwritten
#' if it already exists.
#' @param variables a \code{list}
#' @details
#' The list must follow the following rules. Each element of the list must itself be a list with two keys.
#' The first key must be named "values" and contains a numeric vector. The second key must be named "dimension"
#' and contains a string giving the dimension name. The name of the element itself is used to create the corresponding
#' variable in the NetCDF file.
#' @note
#' Two elements of the given list can possibly have the same dimension name.
#' @return None, but creates a NetCDF file at the specified path.
#' @examples
#' filename <- tempfile(pattern="dummy", fileext=".nc")
#' a <- list(values = 1:3, dimension = "dim_a")
#' b <- list(values = 1:5, dimension = "dim_b")
#' c <- list(values = 5:9, dimension = "dim_b")
#' variables <- list(a=a, b=b, c=c)
#' netcdf_create_from_list(filename, variables)
#' bi_file_ncdump(filename)
#' @export
netcdf_create_from_list <- function(filename, variables){
  filename <- normalizePath(filename, "/", FALSE)
  if (class(variables) != "list"){
    stop("'variables' should be a list")
  }
  dim_size <- list()
  for (name in names(variables)){
    element <- variables[[name]]
    if (class(element) != "list"){
      stop("each element of 'variables' should itself be a list")
    }
    element_names <- names(element)
    if ("dimension" %in% element_names){
      if (class(element[["dimension"]]) != "character"){
        stop("the key 'dimension' of each element of 'variables' should be of type 'character'")
      }
    } else {
      stop("each element of 'variables' should have an element called 'dimension'")
    }
    if ("values" %in% element_names){
      if (!(class(element[["values"]]) %in% c("numeric", "integer"))){
        stop("the key 'values' of each element of 'variables' should be of type 'numeric' or 'integer'")
      }
    } else {
      stop("each element of 'variables' should have an element called 'values'")
    }
    if (element[["dimension"]] %in% names(dim_size)){
      if (length(element[["values"]]) != dim_size[[element[["dimension"]]]]){
        stop("two elements of 'variables' with same dimension name should have equal size")
      }
    } else {
      dim_size[[element[["dimension"]]]] <- length(element[["values"]])
    }
  }
  nc_create_netcdf_from_list_(filename, variables)
}
