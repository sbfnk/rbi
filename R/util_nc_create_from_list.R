#' @rdname netcdf_create_from_list
#' @name netcdf_create_from_list
#' @title Create NetCDF File from R list
#' @description
#' Internal function that creates a NetCDF file given a list.
#' @param filename a path to a NetCDF file to write the variable into, which will be overwritten
#' if it already exists.
#' @param variables a \code{list}
#' @param time_dim the name of the time dimension, if one exists; default: "time"
#' @param coord_dims the names of the coordinate dimension, if any
#' @param value_column if any \code{variables} are data frames, which column contains the values (default: "value")
#' @param guess_time whether to guess time dimension; this would be a numerical column in the data frame given which is not the \code{value_column}; only one such column must exist
#' @param guess_coord whether to guess the coordinate dimension; this would be a column with varying value which is not the time or value column
#' @param verbose if TRUE, will print variables as they are read
#' @details
#'
#' The list of variables must follow the following rules. Each element
#' of the list must itself be one of:
#'
#' 1) a data frame with a \code{value_column} column (see option 'value_column') and any number of other
#' columns indicating one or more dimensions
#'
#' 2) a numeric vector of length one, with no dimensions
#'
#' The name of the list elements itself is used to create the
#' corresponding variable in the NetCDF file.
#'
#' @note Two elements of the given list can possibly have the same
#'   dimension name.
#' @return A list of the time and coord dims, and factors in extra dimensions, if any
#' @importFrom ncdf4 nc_open nc_close ncdim_def ncvar_def nc_create ncvar_put
netcdf_create_from_list <- function(filename, variables, time_dim, coord_dims, value_column = "value", guess_time = FALSE, guess_coord = FALSE, verbose){
  ## get file name
  filename <- normalizePath(filename, "/", FALSE)
  ## argument consistency checks
  if (!("list" %in% class(variables))){
    stop("'variables' should be a list")
  }
  if (is.null(names(variables)) || any(names(variables) == "")) {
    stop("'variables' must be named")
  }
  if (missing(coord_dims) || length(coord_dims) == 0) {
    coord_dims <- NULL
  } else if (guess_coord) {
    stop("'coord_dims' must not be given if guess_cord is TRUE")
  }

  if (missing(time_dim) || length(time_dim) == 0) {
    time_dim <- NULL
  } else if (guess_time) {
    guess_time <- FALSE
  }

  ## initialise variables
  dims <- list() ## dimension variables created with nc_dim
  dim_factors <- list() ## factors for created dimension variables
  vars <- list() ## variables created with nc_var
  values <- list() ## values in the variables
  for (name in names(variables)){ ## loop over list of variables
    if (!missing(verbose) && verbose)
    {
      message(date(), " Preparing ", name)
    }
    ## get list element
    element <- variables[[name]]
    index_cols <- list()
    ## reset time and coord dim if we're guessing
    if (guess_time) time_dim <- NULL
    if (guess_coord) coord_dims <- NULL
    ## reset time index dimension name
    time_index <- NULL
    if (is.data.frame(element)) {
      element <- data.table(element)
      cols <- colnames(element)
      ## check
      if (!(value_column %in% colnames(element))) {
        stop("any elements of 'variables' that are a data frame must have a '", value_column, "' column")
      }
      ## ns dimension gets special treatment
      if ("ns" %in% cols) {
        ns_values <- unique(element[["ns"]])
        ns_dim <- ncdim_def("ns", "", ns_values)
      }
      ## guess time dimension: numeric/integer column that isn't the value column
      if (guess_time) {
        numeric_cols <- cols[which(vapply(cols, function(x) { class(element[[x]]) %in% c("integer", "numeric") }, TRUE))]
        numeric_cols <- setdiff(numeric_cols, value_column)
        if (length(numeric_cols) == 1) {
          time_dim <- numeric_cols
        } else if (length(numeric_cols) > 1){
          if ("time" %in% numeric_cols) {
            time_dim <- "time"
          } else {
            stop("Could not decide on time dimension between ", paste(numeric_cols, collapse=", "))
          }
        }
      }
      ## guess coord dimension(s): a column that is not the time or value column,
      ## and not np or ns
      if (guess_coord) {
        exclude <- c("np", "ns", value_column)
        if (!is.null(time_dim)) {
          exclude <- c(exclude, time_dim)
        }
        guessed_coord <- setdiff(colnames(element), exclude)
        coord_dims <- guessed_coord
      }

      if (is.null(time_dim) && "time" %in% colnames(element)) time_dim <- "time"
      ## add time and coord dimensions to vector of index columns
      if ("ns" %in% colnames(element)) {
        index_cols <- c(index_cols, list(ns = "ns"))
      }
      if (!is.null(time_dim)) {
        index_cols <- c(index_cols, list(time = time_dim))
      }
      if (!is.null(coord_dims)) {
        index_cols <- c(index_cols, list(coord = coord_dims))
      }

      var_dims <- list()
      ## list of dimensions
      ## first, check for time and coord columns
      present_index_cols <- intersect(colnames(element), unlist(index_cols))
      index_table <- unique(element[, present_index_cols, with = FALSE])
      if (nrow(index_table) > 0) {
        nr_table <- index_table
        if ("ns" %in% cols) {
          nr_table <- unique(nr_table[, setdiff(colnames(nr_table), "ns"), with = FALSE])
          var_dims <- c(list(ns_dim), var_dims)
          names(var_dims)[1] <- "ns"
        }
        if (nrow(nr_table) > 0) {
          nr_table[["nr"]] <- seq_len(nrow(nr_table)) - 1
          index_table <- merge(index_table, nr_table)
          time_index <- paste("nr", name, sep = "_")
          nr_dim <- ncdim_def(time_index, "", nr_table[["nr"]])
          dims[[time_index]] <- nr_dim
          var_dims <- c(var_dims, list(nr_dim))
          names(var_dims)[length(var_dims)] <- time_index
        }
        if (!is.null(time_dim) && time_dim %in% cols)
        {
          time_var <- paste("time", name, sep = "_")
          time_dims <- list(nr_dim)
          if ("ns" %in% cols) time_dims <- c(list(ns_dim), time_dims)
          vars[[time_var]] <- ncvar_def(time_var, "", time_dims)
          values[[time_var]] <- index_table[[time_dim]]
        }
        if (!is.null(coord_dims) &&
            length(intersect(coord_dims, cols)) > 0) {
          coord_var <- paste("coord", name, sep = "_")
          if (length(coord_dims) > 1) {
            coord_index <- paste("index", coord_var, sep = "_")
            coord_index_values <- seq_along(coord_dims) - 1
            coord_index_dim <- ncdim_def(coord_index, "", coord_index_values)
            dims[[coord_index]] <- coord_index_dim
          }
          for (coord_dim in coord_dims) {
            if (!any(class(index_table[[coord_dim]]) %in% c("numeric", "integer") &&
                     length(setdiff(as.integer(index_table[[coord_dim]]), index_table[[coord_dim]])) == 0 &&
                     length(setdiff(seq_len(max(index_table[[coord_dim]])), unique(index_table[[coord_dim]]))) == 0))
            {
              if (any(class(index_table[[coord_dim]]) == "factor"))
              {
                dim_factors[[coord_dim]] <- union(dim_factors[[coord_dim]], levels(index_table[[coord_dim]]))
              } else
              {
                dim_factors[[coord_dim]] <- union(dim_factors[[coord_dim]], unique(index_table[[coord_dim]]))
              }
              index_table[[coord_dim]] <-
                as.integer(factor(index_table[[coord_dim]],
                                  levels = dim_factors[[coord_dim]])) - 1
            }
          }

          coord_var_dims <- dims[sub("^coord", "nr", coord_var)]
          if (length(coord_dims) > 1) {
            sort_keys <- c(coord_dims)
            index_cols <- colnames(index_table)
            if ("nr" %in% index_cols) sort_keys <- c(sort_keys, "nr")
            if ("ns" %in% index_cols) sort_keys <- c(sort_keys, "ns")
            setkeyv(index_table, sort_keys)
            index_table[[coord_var]] <- seq_len(nrow(index_table))
            id_columns <- c()
            if ("nr" %in% index_cols) id_columns <- c("nr", id_columns)
            if ("ns" %in% index_cols) id_columns <- c("ns", id_columns)
            coord_table <-
              data.table::melt(index_table[, c(id_columns, coord_dims),
                                           with = FALSE], id.vars=id_columns)
            sort_keys <- rev(setdiff(colnames(coord_table), "value"))
            setkeyv(coord_table, sort_keys)
            values[[coord_var]] <- coord_table[[value_column]]
            coord_var_dims <- c(coord_var_dims, list(coord_index_dim))
          } else
          {
            values[[coord_var]] <- index_table[[coord_dims]]
          }
          if ("ns" %in% cols) {
            coord_var_dims <- c(list(ns_dim), coord_var_dims)
          }
          vars[[coord_var]] <- ncvar_def(coord_var, "", coord_var_dims)
        }
      }
      data_cols <- setdiff(cols, c(unlist(index_cols), value_column))
      if ("time" %in% data_cols) {
        stop("Can't have a time dimension called 'time'; try setting 'time_dim = \"time\"'.")
      }
      if ("coord" %in% data_cols) {
        stop("Can't have a coord dimension called 'coord'; try setting 'coord_dims = \"coord\"'.")
      }

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
      order_cols <- data_cols
      if (!is.null(time_index)) order_cols <- c(order_cols, time_index)
      if ("ns" %in% cols) order_cols <- c(order_cols, "ns")
      var_dims <-
        var_dims[names(var_dims)[order(match(names(var_dims), order_cols))]]

      table_order <- rev(names(var_dims))
      if (!is.null(time_index)) table_order <- c(time_dim, table_order)

      new_order <- lapply(intersect(table_order, colnames(element)), function(x) {element[[x]]})
      if (length(new_order) > 0) {
        element <- element[do.call(order, new_order), ]
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

  nc <- nc_create(filename, vars)

  for (name in names(vars)) {
    if (!missing(verbose) && verbose)
    {
      message(date(), " Writing ", name)
    }
    ncvar_put(nc, vars[[name]], values[[name]])
  }

  nc_close(nc)

  return(list(time_dim=time_dim, coord_dims=coord_dims, dims=dim_factors))
}
