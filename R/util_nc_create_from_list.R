#' @rdname netcdf_create_from_list
#' @name netcdf_create_from_list
#' @aliases netcdf_create_from_list
#' @title Create NetCDF File from R list
#' @description
#' This function creates a NetCDF file given a list.
#' @param filename a path to a NetCDF file to write the variable into, which will be overwritten
#' if it already exists.
#' @param variables a \code{list}
#' @param time_dim the name of the time dimension, if one exists
#' @details
#'
#' The list of variables must follow the following rules. Each element
#' of the list must itself be one of:
#'
#' 1) a list with two keys; the first key must be named "values" and
#' contains a numeric vector; the second key must be named "dimension"
#' and contains a string giving the dimension name.
#'
#' 2) a data frame with a "value" column and any number of other
#' columns indicating one or more dimensions
#'
#' 3) a numeric vector of length one, with no dimensions
#'
#' The name of the list elements itself is used to create the
#' corresponding variable in the NetCDF file.
#' 
#' @note Two elements of the given list can possibly have the same
#'   dimension name.
#' @return None, but creates a NetCDF file at the specified path.
#' @examples
#' filename <- tempfile(pattern="dummy", fileext=".nc")
#' a <- list(values = 1:3, dimension = "dim_a")
#' b <- list(values = 1:5, dimension = "dim_b")
#' c <- list(values = 5:9, dimension = "dim_b")
#' d <- 3
#' e <- data.frame(dim_a = rep(1:3, time = 2), dim_c = rep(1:2, each = 3), value = 1:6)
#' variables <- list(a=a, b=b, c=c, d=d, e=e)
#' netcdf_create_from_list(filename, variables)
#' bi_file_ncdump(filename)
#' @export
netcdf_create_from_list <- function(filename, variables, time_dim){
  filename <- normalizePath(filename, "/", FALSE)
  if (class(variables) != "list"){
    stop("'variables' should be a list")
  }
  if (is.null(names(variables)) || any(names(variables) == "")) {
    stop("'variables' must be named")
  }
  dims <- list()
  vars <- list()
  values <- list()
  for (name in names(variables)){
    element <- variables[[name]]
    if ("list" %in% class(element)) {
      element_names <- names(element)
      if ("dimension" %in% element_names){
        if (class(element[["dimension"]]) != "character"){
          stop("the key 'dimension' of each element of 'variables' should be of type 'character'")
        }
      } else {
        stop("if an element of 'variables' is a list, it should have an element called 'dimension'")
      }
      if ("values" %in% element_names){
        if (!(class(element[["values"]]) %in% c("numeric", "integer"))){
          stop("the key 'values' of each element of 'variables' should be of type 'numeric' or 'integer'")
        }
      } else {
        stop("each element of 'variables' should have an element called 'values'")
      }
      if (element[["dimension"]] %in% names(dims)){
        if (length(element[["values"]]) != dims[[element[["dimension"]]]]$len){
          stop("two elements of 'variables' with same dimension name should have equal size")
        }
      } else {
        new_dim <- ncdim_def(element[["dimension"]], "", seq_along(element[["values"]]) - 1)
        dims[[element[["dimension"]]]] <- new_dim
      }
      vars[[name]] <- ncvar_def(name, "", dims[[element[["dimension"]]]])
      values[[name]] <- element[["values"]]
    } else if (length(intersect(class(element), c("data.frame"))) > 0) {
      if (!("value" %in% colnames(element))) {
        stop("any elements of 'variables' that are a data frame must have a 'value' column")
      }
      var_dims <- list()
      for (col in rev(colnames(element)[colnames(element) != "value"])) {
	dim_name <- ifelse(!missing(time_dim) && col == time_dim, "nr", col)
        dim_values <- seq_along(unique(element[, col])) - 1
        if (dim_name %in% names(dims)) {
          if (length(dim_values) != dims[[dim_name]]$len){
            stop("two elements of 'variables' with same dimension name (", dim_name, ") should have equal size")
          }
        } else {
          new_dim <- ncdim_def(dim_name, "", dim_values)
          dims[[dim_name]] <- new_dim
        }
        if (!((dim_name == "nr") && ("nr" %in% names(var_dims)))) {
            ## make sure there is only one 'nr' dim per variable
          var_dims <- c(var_dims, list(dims[[dim_name]]))
          names(var_dims)[length(var_dims)] <- dim_name
        }
      }
      if (!missing(time_dim) && time_dim %in% colnames(element)) {
	## time_var <- paste("time", name, sep = "_")
	time_var <- "time"
        vars[[time_var]] <- ncvar_def(time_var, "", list(dims[["nr"]]))
        values[[time_var]] <- unique(element[, time_dim])
      }
      vars[[name]] <- ncvar_def(name, "", var_dims)
      values[[name]] <- element[do.call(order, element[rev(names(var_dims))]), "value"]
    } else if (length(intersect(class(element), c("numeric", "integer"))) > 0) {
      if (length(element) > 1) {
        stop("any elements of 'variables' that are a vector must be of length 1")
      }
      vars[[name]] <- ncvar_def(name, "", list())
      values[[name]] <- element
    } else {
      stop("each element of 'variables' should itself be a list or a data frame, or a numeric vector of length 1")
    }
  }

  nc <- nc_create(filename, vars)

  for (name in names(vars)) {
    ncvar_put(nc, vars[[name]], values[[name]])
  }

  nc_close(nc)
          
  ## nc_create_netcdf_from_list_(filename, variables)
  
}
