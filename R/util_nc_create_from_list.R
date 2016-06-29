#' @rdname netcdf_create_from_list
#' @name netcdf_create_from_list
#' @aliases netcdf_create_from_list
#' @title Create NetCDF File from R list
#' @description
#' Internal function that creates a NetCDF file given a list.
#' @param filename a path to a NetCDF file to write the variable into, which will be overwritten
#' if it already exists.
#' @param variables a \code{list}
#' @param time_dim the name of the time dimension, if one exists
#' @param coord_dim the name of the coordinate dimension,  if one exists
#' @param value_column if any \code{variables} are data frames, which column contains the values (default: "value")
#' @param guess_time whether to guess time dimension; this would be a numerical column in the data frame given which is not the \code{value_column}; only one such column must exist
#' @param guess_coord whether to guess the coordinate dimension; this would be a column with varying value which is not the time or value column
#' @details
#'
#' The list of variables must follow the following rules. Each element
#' of the list must itself be one of:
#'
#' 1) a list with two keys; the first key must be named "values" and
#' contains a numeric vector; the second key must be named "dimension"
#' and contains a string giving the dimension name.
#'
#' 2) a data frame with a \code{value_column} column (see option 'value_column') and any number of other
#' columns indicating one or more dimensions
#'
#' 3) a numeric vector of length one, with no dimensions
#'
#' The name of the list elements itself is used to create the
#' corresponding variable in the NetCDF file.
#' 
#' @note Two elements of the given list can possibly have the same
#'   dimension name.
#' @return A list of factors in extra dimensions, if any
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
#' @importFrom ncdf4 nc_open nc_close ncdim_def ncvar_def nc_create ncvar_put
netcdf_create_from_list <- function(filename, variables, time_dim, coord_dim, value_column = "value", guess_time = FALSE, guess_coord = FALSE){
  ## get file name
  filename <- normalizePath(filename, "/", FALSE)
  ## argument consistency checks
  if (!("list" %in% class(variables))){
    stop("'variables' should be a list")
  }
  if (is.null(names(variables)) || any(names(variables) == "")) {
    stop("'variables' must be named")
  }
  if (missing(coord_dim)) {
    coord_dim <- NULL
  } else if (guess_coord) {
    stop("'coord_dim' must not be given is guess_cord is TRUE")
  }

  if (missing(time_dim)) {
    time_dim <- NULL
  } else if (guess_time) {
    stop("'time_dim' must not be given is guess_cord is TRUE")
  }

  ## initialise variables
  dims <- list() ## dimension variables created with nc_dim
  dim_factors <- list() ## factors for created dimension variables
  vars <- list() ## variables created with nc_var
  values <- list() ## values in the variables
  for (name in names(variables)){ ## loop over list of variables
    ## get list element
    element <- variables[[name]]
    index_cols <- c()
    ## reset time and coord dim if we're guessing
    if (guess_time) time_dim <- NULL
    if (guess_coord) coord_dim <- NULL
    ## reset time index dimension name
    time_index <- NULL
    if ("list" %in% class(element)) { ## element is a list of values and dimensions
      element_names <- names(element)
      ## checks
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
        ## define dimension if given
        dim_name <- element[["dimension"]]
        dim_values <- element[["values"]]
        if (class(element[["values"]]) %in% c("character", "factor")) {
          dim_factors[[dim_name]] <- union(dim_factors[[dim_name]], dim_values)
        }
      }
      ## get variable values
      vars[[name]] <- ncvar_def(name, "", dims[[element[["dimension"]]]])
      values[[name]] <- element[["values"]]
    } else if (length(intersect(class(element), c("data.frame"))) > 0) { # element is a data frame
      cols <- colnames(element)
      ## check
      if (!(value_column %in% colnames(element))) {
        stop("any elements of 'variables' that are a data frame must have a '", value_column, "' column")
      }
      ## guess time dimension: numeric/integer column that isn't the value column
      if (guess_time) {
        numeric_cols <- cols[which(sapply(cols, function(x) { class(element[[x]]) %in% c("integer", "numeric") }))]
        numeric_cols <- setdiff(numeric_cols, value_column)
        if (length(numeric_cols) == 1) {
          time_dim <- numeric_cols
        } else if (length(numeric_cols) > 1){
          stop("Could not decide on time dimension between ", numeric_cols)
        }
      }
      ## guess coord dimension: a column that is not the time or value column
      if (guess_coord) {
        exclude <- value_column
        if (!is.null(time_dim)) {
          exclude <- c(exclude, time_dim)
        }
        guessed_coord <- setdiff(colnames(element), exclude)
        if (length(guessed_coord) == 1) { ## found a unique coord dimension
          coord_dim <- guessed_coord
        } else if (length(guessed_coord) > 1){
          stop("Could not decide on coord dimension between ", guessed_coord)
        }

      }
      ## add time and coord dimensions to vector of index columns
      if (!is.null(time_dim)) index_cols <- c(index_cols, time = time_dim)
      if (!is.null(coord_dim)) index_cols <- c(index_cols, coord = coord_dim)

       var_dims <- list()
      ## list of dimensions
      ## first, check for time and coord columns
      index_table <-
        unique(as.data.frame(element)[, intersect(colnames(element), index_cols), drop = FALSE])
      if (nrow(index_table) > 0) {
        nr_values <- seq_len(nrow(index_table)) - 1
        time_index <- paste("nr", name, sep = "_")
        nr_dim <- ncdim_def(time_index, "", nr_values)
        dims[[time_index]] <- nr_dim
        var_dims <- c(var_dims, list(nr_dim))
        names(var_dims)[length(var_dims)] <- time_index

        if (!is.null(time_dim) && time_dim %in% cols)
        {
          time_var <- paste("time", name, sep = "_")
          vars[[time_var]] <- ncvar_def(time_var, "", list(nr_dim))
          values[[time_var]] <- index_table[[time_dim]]
        }
        if (!is.null(coord_dim) && coord_dim %in% cols) {
          coord_var <- paste("coord", name, sep = "_")
          values[[coord_var]] <- index_table[[coord_dim]]
          if (!(class(index_table[[coord_dim]]) %in% c("numeric", "integer") &&
                length(setdiff(as.integer(index_table[[coord_dim]]), index_table[[coord_dim]])) == 0 &&
                length(setdiff(seq_len(max(index_table[[coord_dim]])), unique(index_table[[coord_dim]]))) == 0))
          {
            dim_factors[[coord_dim]] <- union(dim_factors[[coord_dim]], unique(index_table[[coord_dim]]))
          }
        }
      }
      data_cols <- setdiff(cols, c(index_cols, value_column))
      for (col in rev(data_cols)) {
        dim_name <- col
        ## strip trailing numbers, these indicate duplicate dimensions
        dim_name <- sub("\\.[0-9]+$", "", dim_name)
        dim_values <- unique(element[[col]])
        if (dim_name %in% names(dims)) {
          if (length(dim_values) != dims[[dim_name]]$len) {
            stop("Two dimensions of name '", dim_name, "' have different lengths")
          }
        } else {
          new_dim <- ncdim_def(dim_name, "", seq_along(unique(dim_values)) - 1)
          dims[[dim_name]] <- new_dim
          if (!(class(dim_values) %in% c("numeric", "integer") &&
                length(setdiff(as.integer(dim_values), dim_values)) == 0 &&
                length(setdiff(seq_len(max(dim_values)), unique(dim_values))) == 0))
          {
            dim_factors[[dim_name]] <- dim_values
          }
        }

        var_dims <- c(var_dims, list(dims[[dim_name]]))
        names(var_dims)[length(var_dims)] <- col
      }
      ## order variables
      if (!is.null(time_index)) {
        order_cols <- rev(c(time_index, cols))
        var_dims <-
          var_dims[names(var_dims)[order(match(names(var_dims), order_cols))]]
      }
      vars[[name]] <- ncvar_def(name, "", var_dims)
      values[[name]] <- element[[value_column]]
    } else if (length(intersect(typeof(element), c("double", "integer"))) > 0) {
      if (length(element) > 1) {
        stop("any elements of 'variables' that are a vector must be of length 1")
      }
      vars[[name]] <- ncvar_def(name, "", list())
      values[[name]] <- element
    } else {
      stop("each element of 'variables' should itself be a list or a data frame, or a numeric vector of length 1")
    }
  }

  ## factorise coord variable
  if (!is.null(coord_dim)) {
    for (coord_var in grep("^coord_", names(values), value = TRUE)) {
      if (coord_dim %in% names(dim_factors)) {
        values[[coord_var]] <- as.integer(factor(values[[coord_var]], levels = dim_factors[[coord_dim]])) - 1
      }

      var_dims <- dims[sub("^coord", "nr", coord_var)]
      vars[[coord_var]] <- ncvar_def(coord_var, "", var_dims)
    }
  }
  nc <- nc_create(filename, vars)

  for (name in names(vars)) {
    ncvar_put(nc, vars[[name]], values[[name]])
  }

  nc_close(nc)

  if (length(dim_factors) > 0) return(dim_factors)
}
