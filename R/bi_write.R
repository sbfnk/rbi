#' @rdname bi_write
#' @name bi_write
#' @title Create (e.g., init or observation) files for LibBi
#' @description
#' This function creates (or appends to) a NetCDF file for LibBi from the given
#'   list of vectors and/or data frames. Since any files can be passed to
#'   \code{\link{libbi}} directly via the \code{init}, \code{input} and
#'   \code{obs} options, this is mostly used internally, this is mostly used
#'   internally.
#' @param filename a path to a NetCDF file to write the variables into, which
#'   will be overwritten if it already exists. If necessary, ".nc" will be added
#'   to the file name
#' @param variables a \code{list} object, the names of which should be the variable names and values should be either single values or data frames
#' @param timed deprecated; timed variables should be given as data frames
#' @param append if TRUE, will append variables if file exists (default: FALSE)
#' @param time_dim the name of the time dimension, if one exists; default: "time"
#' @param coord_dims the names of the coordinate dimension, if any; should be a named list of character vectors, they are matched to variables names
#' @param dim_factors factors that dimensions have; this corresponds to the \code{dims} element of a \code{\link{libbi}} object
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
#' @return A list of the time and coord dims, and factors in extra dimensions, if any
#' @importFrom ncdf4 nc_close ncdim_def ncvar_def nc_create ncvar_put ncvar_add
#' @examples
#' filename <- tempfile(pattern="dummy", fileext=".nc")
#' a <- 3
#' b <- data.frame(dim_a = rep(1:3, time = 2), dim_b = rep(1:2, each = 3), value = 1:6)
#' variables <- list(a=a, b=b)
#' bi_write(filename, variables)
#' bi_file_summary(filename)
#' @export
bi_write <- function(filename, variables, timed, append=FALSE, time_dim, coord_dims, dim_factors, value_column = "value", guess_time = FALSE, guess_coord = FALSE, verbose) {

  if (!grepl("\\.nc$", filename)) {
    filename <- paste(filename, "nc", sep = ".")
  }

  if (!missing(timed)) {
    stop("'timed' is deprecated; all timed variables should be given as data frames in 'variables'")
  }

  if (!("list" %in% class(variables)) || length(variables) == 0){
    stop("please provide a non-empty list to bi_write")
  }

  if (is.null(names(variables)) || any(names(variables) == "")) {
    stop("'variables' must be named")
  }

  if (missing(coord_dims) || length(coord_dims) == 0) {
    coord_dims <- list()
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
  if (missing(dim_factors)) dim_factors <- list() ## factors for created dimension variables
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
    ## reset nr index dimension name
    nr_index <- NULL
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
      if (is.null(time_dim) && "time" %in% colnames(element)) time_dim <- "time"
      ## guess coord dimension(s): a column that is not the time or value column,
      ## and not np or ns
      if (guess_coord) {
        exclude <- c("np", "ns", value_column)
        if (!is.null(time_dim)) {
          exclude <- c(exclude, time_dim)
        }
        guessed_coord <- setdiff(colnames(element), exclude)
        if (length(guessed_coord) > 0) coord_dims[[name]] <- guessed_coord
      }

      ## add time and coord dimensions to vector of index columns
      if ("ns" %in% colnames(element)) {
        index_cols <- c(index_cols, list(ns = "ns"))
      }
      if (!is.null(time_dim)) {
        index_cols <- c(index_cols, list(time = time_dim))
      }
      if (!is.null(coord_dims[[name]])) {
        index_cols <- c(index_cols, list(coord = coord_dims[[name]]))
      }

      var_dims <- list()
      ## list of dimensions
      ## first, check for time and coord columns
      present_index_cols <- intersect(colnames(element), unlist(index_cols))
      index_table <- unique(element[, present_index_cols, with = FALSE])
      if (nrow(index_table) > 0) {
        setkeyv(index_table, unlist(index_cols))
      }
      nr_table <- data.table::copy(index_table)
      if (nrow(nr_table) > 0) {
        if ("ns" %in% cols) {
          nr_table <- unique(nr_table[, setdiff(colnames(nr_table), "ns"), with = FALSE])
          var_dims <- c(list(ns_dim), var_dims)
          names(var_dims)[1] <- "ns"
        }
        if (nrow(nr_table) > 0) {
          nr_index <- paste("nr", name, sep = "_")
          nr_table[[nr_index]] <- seq_len(nrow(nr_table)) - 1
          index_table <- merge(index_table, nr_table, by=present_index_cols)
          setkeyv(index_table, nr_index)
          nr_dim <- ncdim_def(nr_index, "", nr_table[[nr_index]])
          dims[[nr_index]] <- nr_dim
          var_dims <- c(var_dims, list(nr_dim))
          names(var_dims)[length(var_dims)] <- nr_index
        }
        if (!is.null(time_dim) && time_dim %in% cols)
        {
          time_var <- paste("time", name, sep = "_")
          time_dims <- list(nr_dim)
          if ("ns" %in% cols) time_dims <- c(list(ns_dim), time_dims)
          vars[[time_var]] <- ncvar_def(time_var, "", time_dims)
          values[[time_var]] <- index_table[[time_dim]]
        }
        if (!is.null(coord_dims[[name]]) &&
            length(intersect(coord_dims[[name]], cols)) > 0) {
          coord_var <- paste("coord", name, sep = "_")
          if (length(coord_dims[[name]]) > 1) {
            coord_index <- paste("index", coord_var, sep = "_")
            coord_index_values <- seq_along(coord_dims[[name]]) - 1
            coord_index_dim <- ncdim_def(coord_index, "", coord_index_values)
            dims[[coord_index]] <- coord_index_dim
          }
          for (coord_dim in coord_dims[[name]]) {
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
          if (length(coord_dims[[name]]) > 1) {
            sort_keys <- c(coord_dims[[name]])
            index_cols <- colnames(index_table)
            if (nr_index %in% index_cols) sort_keys <- c(nr_index, sort_keys)
            if ("ns" %in% index_cols) sort_keys <- c(sort_keys, "ns")
            setkeyv(index_table, sort_keys)
            id_columns <- c()
            if (nr_index %in% index_cols) id_columns <- c(nr_index, id_columns)
            if ("ns" %in% index_cols) id_columns <- c("ns", id_columns)
            if (length(id_columns) > 0) {
              coord_table <-
                data.table::melt(index_table[, c(id_columns, coord_dims[[name]]),
                                             with = FALSE], id.vars=id_columns)
            }
            sort_keys <- rev(setdiff(colnames(coord_table), "value"))
            setkeyv(coord_table, sort_keys)
            values[[coord_var]] <- coord_table[[value_column]]
            coord_var_dims <- c(coord_var_dims, list(coord_index_dim))
          } else
          {
            values[[coord_var]] <- index_table[[coord_dims[[name]]]]
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
      if (!is.null(nr_index)) order_cols <- c(order_cols, nr_index)
      if ("ns" %in% cols) order_cols <- c(order_cols, "ns")
      var_dims <-
        var_dims[names(var_dims)[order(match(names(var_dims), order_cols))]]

      table_order <- rev(names(var_dims))
      if (nrow(nr_table) > 0) {
        element <- merge(element, nr_table, by=present_index_cols)
      }

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

  if (append && file.exists(filename)) {
    nc <- nc_open(filename, write=TRUE)
    for (name in names(vars)) ncvar_add(nc, vars[[name]])
  } else {
    nc <- nc_create(filename, vars)
  }
  existing_vars <- unname(vapply(nc[["var"]], function(y) { y[["name"]] }, ""))

  for (name in names(vars)) {
    if (!missing(verbose) && verbose)
    {
      message(date(), " Writing ", name)
    }
    values[[name]][!is.finite(values[[name]])] <- NA_real_
    ## create an additional variable if needed
    ncvar_put(nc, name, values[[name]])
  }

  nc_close(nc)

  return(list(time_dim=time_dim, coord_dims=coord_dims, dims=dim_factors))

}

