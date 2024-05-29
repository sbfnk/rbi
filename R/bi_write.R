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
#' @param variables a \code{list} object, the names of which should be the
#'   variable names and values should be either single values or data frames
#' @param append if TRUE, will append variables if file exists; default: FALSE
#' @param overwrite if TRUE, will overwrite variables if file exists; default:
#'   FALSE
#' @param time_dim the name of the time dimension, if one exists; default:
#'   "time"
#' @param coord_dims the names of the coordinate dimension, if any; should be a
#'   named list of character vectors, they are matched to variables names
#' @param dim_factors factors that dimensions have; this corresponds to the
#'   \code{dims} element of a \code{\link{libbi}} object
#' @param value_column if any \code{variables} are data frames, which column
#'   contains the values (default: "value")
#' @param guess_time whether to guess time dimension; this would be a numerical
#'   column in the data frame given which is not the \code{value_column}; only
#'   one such column must exist
#' @param verbose if TRUE, will print variables as they are read
#' @details
#'
#' The list of variables must follow the following rules. Each element
#' of the list must itself be one of:
#'
#' 1) a data frame with a \code{value_column} column (see option 'value_column')
#'   and any number of other columns indicating one or more dimensions
#'
#' 2) a numeric vector of length one, with no dimensions
#'
#' The name of the list elements itself is used to create the
#' corresponding variable in the NetCDF file.
#'
#' @return A list of the time and coord dims, and factors in extra dimensions,
#'   if any
#' @importFrom ncdf4 nc_close ncdim_def ncvar_def nc_create ncvar_put ncvar_add
#' @importFrom data.table data.table copy rbindlist
#' @importFrom reshape2 melt
#' @examples
#' filename <- tempfile(pattern = "dummy", fileext = ".nc")
#' a <- 3
#' b <- data.frame(
#'   dim_a = rep(1:3, time = 2), dim_b = rep(1:2, each = 3), value = 1:6
#' )
#' variables <- list(a = a, b = b)
#' bi_write(filename, variables)
#' bi_file_summary(filename)
#' @export
bi_write <- function(filename, variables, append = FALSE, overwrite = FALSE,
                     time_dim, coord_dims, dim_factors, value_column = "value",
                     guess_time = FALSE, verbose) {
  if (!grepl("\\.nc$", filename)) {
    filename <- paste(filename, "nc", sep = ".")
  }

  if (!("list" %in% class(variables)) || length(variables) == 0) {
    stop("please provide a non-empty list to bi_write")
  }

  if (is.null(names(variables)) || any(names(variables) == "")) {
    stop("'variables' must be named")
  }

  if (missing(coord_dims) || length(coord_dims) == 0) {
    coord_dims <- list()
  }

  if (missing(time_dim) || length(time_dim) == 0) {
    time_dim <- NULL
  } else if (guess_time) {
    stop("'time_dim' must not be given if guess_time is TRUE")
  }

  ## initialise variables
  dims <- list() ## dimension variables created with nc_dim
  if (missing(dim_factors)) {
    dim_factors <- list() ## factors for created dimension variables
  }
  vars <- list() ## variables created with nc_var
  values <- list() ## values in the variables
  for (name in names(variables)) { ## loop over list of variables
    if (!missing(verbose) && verbose) {
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
      ## attach numbers to duplicated columnns
      duplicated_cols <- unique(cols[duplicated(cols)])
      for (col in duplicated_cols) {
        new_colnames <- paste(col, seq_along(cols[cols == col]), sep = ".")
        setnames(element, which(cols == col), new_colnames)
        cols[cols == col] <- new_colnames
      }
      ## check
      if (!(value_column %in% colnames(element))) {
        stop(
          "any elements of 'variables' that are a data frame must have a '",
          value_column, "' column"
        )
      }
      ## ns dimension gets special treatment
      if ("ns" %in% cols) {
        ns_values <- unique(element[["ns"]])
        ns_dim <- ncdim_def("ns", "", ns_values)
      } else {
        ns_dim <- NULL
      }
      ## guess time dimension: numeric/integer column that isn't the value
      ## column
      if (guess_time) {
        numeric_cols <- cols[which(vapply(cols, function(x) {
          class(element[[x]]) %in% c("integer", "numeric")
        }, TRUE))]
        numeric_cols <- setdiff(numeric_cols, value_column)
        if (length(numeric_cols) == 1) {
          time_dim <- numeric_cols
        } else if (length(numeric_cols) > 1) {
          if ("time" %in% numeric_cols) {
            time_dim <- "time"
            guess_time <- FALSE
          } else {
            stop(
              "Could not decide on time dimension between ",
              paste(numeric_cols, collapse = ", ")
            )
          }
        }
      }
      if (is.null(time_dim) && "time" %in% colnames(element)) time_dim <- "time"
      ## guess coord dimension(s): a column that is not the time or value
      ## column, and not np or ns; this is only used later if the data is
      ## sparse
      exclude <- c("np", "ns", value_column)
      if (!is.null(time_dim)) {
        exclude <- c(exclude, time_dim)
      }
      guessed_coord <- setdiff(colnames(element), exclude)
      if (length(guessed_coord) > 0) coord_dims[[name]] <- guessed_coord

      ## add time and coord dimensions to vector of index columns
      if ("ns" %in% colnames(element)) {
        index_cols <- c(index_cols, list(ns = "ns"))
      }
      if (!is.null(time_dim)) {
        index_cols <- c(index_cols, list(time = time_dim))
      }
      if (!is.null(coord_dims[[name]])) {
        sparse <- check_sparse_var(element, coord_dims[[name]], value_column)
        if (sparse) {
          index_cols <- c(index_cols, list(coord = coord_dims[[name]]))
          ## strip trailing numbers, these indicate duplicate dimensions
          check_coord_dims <- sub("\\.[0-9]+$", "", coord_dims[[name]])
          if (any(duplicated(check_coord_dims))) {
            stop("Sparse duplicated dimensions are not supported.")
          }
        }

      } else {
        sparse <- FALSE
      }

      var_dims <- list()
      ## list of dimensions
      ## first, check for time and coord columns
      present_index_cols <- intersect(unlist(index_cols), colnames(element))
      index_table <- unique(element[, present_index_cols, with = FALSE])
      if (nrow(index_table) > 0) {
        setkeyv(index_table, unlist(present_index_cols))
      }
      nr_table <- copy(index_table)
      if (nrow(index_table) > 0) {
        if ("ns" %in% cols) {
          nr_table <- unique(
            nr_table[, setdiff(colnames(nr_table), "ns"), with = FALSE]
          )
          var_dims <- c(list(ns_dim), var_dims)
          names(var_dims)[1] <- "ns"
          present_index_cols <- setdiff(present_index_cols, "ns")
        }
        if (nrow(nr_table) > 0) {
          nr_index <- paste("nr", name, sep = "_")
          nr_table[[nr_index]] <- seq_len(nrow(nr_table)) - 1
          index_table <- merge(index_table, nr_table, by = present_index_cols)
          setkeyv(index_table, nr_index)
          nr_dim <- ncdim_def(nr_index, "", nr_table[[nr_index]])
          dims[[nr_index]] <- nr_dim
          var_dims <- c(var_dims, list(nr_dim))
          names(var_dims)[length(var_dims)] <- nr_index
        }
        if (sparse) {
          coord <- create_coord_var(
            name, dims, dim_factors, coord_dims[[name]], index_table, ns_dim,
            time_dim, nr_index, value_column
          )
          values[[coord[["name"]]]] <- coord[["values"]]
          vars[[coord[["name"]]]] <- coord[["var"]]
          dims <- c(dims, coord[["dim"]])
          for (dim in names(coord[["dim_factors"]])) {
            dim_factors[[dim]] <- coord[["dim_factors"]][[dim]]
          }
        }
        if (!is.null(time_dim) && time_dim %in% cols) {
          time_var <- paste("time", name, sep = "_")
          time_dims <- list(nr_dim)
          if ("ns" %in% cols) time_dims <- c(list(ns_dim), time_dims)
          vars[[time_var]] <- ncvar_def(time_var, "", time_dims)
          values[[time_var]] <- index_table[[time_dim]]
        }
      }
      data_cols <- setdiff(cols, c(unlist(index_cols), value_column))
      if ("time" %in% data_cols) {
        stop(
          "Can't have a time dimension called 'time'; try setting ",
          "'time_dim = \"time\"'."
        )
      }
      if ("coord" %in% data_cols) {
        stop(
          "Can't have a coord dimension called 'coord'; try setting ",
          "'coord_dims = \"coord\"'."
        )
      }

      for (col in rev(data_cols)) {
        dim_name <- col
        ## strip trailing numbers, these indicate duplicate dimensions
        dim_name <- sub("\\.[0-9]+$", "", dim_name)
        if (is.factor(element[[col]])) {
          dim_values <- levels(element[[col]])
        } else {
          dim_values <- unique(element[[col]])
        }
        if (dim_name %in% names(dims)) {
          if (length(dim_values) != dims[[dim_name]]$len) {
            stop(
              "Two dimensions of name '", dim_name, "' have different lengths"
            )
          }
        } else {
          new_dim <- ncdim_def(dim_name, "", seq_along(dim_values) - 1)
          dims[[dim_name]] <- new_dim
          if (!(class(dim_values) %in% c("numeric", "integer") &&
            length(setdiff(as.integer(dim_values), dim_values)) == 0 &&
            length(setdiff(
              seq_len(max(dim_values)), unique(dim_values)
            )) == 0)) {
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
        element <- merge(element, nr_table, by = present_index_cols)
      }

      new_order <- lapply(
        intersect(table_order, colnames(element)), function(x) {
          element[[x]]
        }
      )
      if (length(new_order) > 0) {
        element <- element[do.call(order, new_order), ]
      }

      vars[[name]] <- ncvar_def(name, "", var_dims)
      values[[name]] <- element[[value_column]]
    } else if (length(intersect(typeof(element), c("double", "integer"))) > 0) {
      if (length(element) > 1) {
        stop(
          "any elements of 'variables' that are a vector must be of length 1"
        )
      }
      vars[[name]] <- ncvar_def(name, "", list())
      values[[name]] <- element
    } else {
      stop(
        "each element of 'variables' should itself be a list or a data frame, ",
        "or a numeric vector of length 1"
      )
    }
  }

  if ((append || overwrite) && file.exists(filename)) {
    nc <- nc_open(filename, write = TRUE)
    existing_vars <- unname(vapply(nc[["var"]], function(y) {
      y[["name"]]
    }, ""))
    if (append) {
      for (name in setdiff(names(vars), existing_vars)) {
        nc <- ncvar_add(nc, vars[[name]])
      }
    }
  } else {
    nc <- nc_create(filename, vars)
    existing_vars <- c()
  }

  for (name in names(vars)) {
    if ((!(append || overwrite)) ||
      (append && !(name %in% existing_vars)) ||
      (overwrite && (name %in% existing_vars))) {
      if (!missing(verbose) && verbose) {
        message(date(), " Writing ", name)
      }
      values[[name]][!is.finite(values[[name]])] <- NA_real_
      ## create an additional variable if needed
      ncvar_put(nc, name, values[[name]])
    }
  }

  nc_close(nc)

  return(list(time_dim = time_dim, coord_dims = coord_dims, dims = dim_factors))
}

##' Check if a variable is sparse
##'
##' Takes a data.table with given coordinate columns and a value column and
##' checks if all combinations of the coordinate columns are present for each
##' combination of the other columns.
##' @title Check if a variable is sparse
##' @param x data.table
##' @param coord_cols character vector of coordinate columns
##' @param value_column the name of the value column
##' @return TRUE if the variable is sparse, FALSE otherwise
##' @importFrom data.table copy CJ setorderv .SD
##' @keywords internal
##' @author Sebastian Funk
check_sparse_var <- function(x, coord_cols, value_column) {
  ## for each combination of coord_cols check if all combinations of values
  ## in other columns except value_column are present
  check <- copy(x)
  other_cols <- setdiff(names(check), c(coord_cols, value_column))
  setorderv(check, coord_cols)

  all_values <- lapply(coord_cols, function(x) unique(check[[x]]))
  names(all_values) <- coord_cols
  all_combinations <- do.call(CJ, all_values)

  ## check if for all combinations of other calls the values of coord_cols
  ## are all available combinations
  all <- check[, list(
    all_equal = nrow(.SD) == nrow(all_combinations) &&
      all(.SD[, coord_cols, with = FALSE] == all_combinations)
  ), by = other_cols]

  ## all_factors
  all_factors <- vapply(coord_cols, function(x) {
    length(setdiff(levels(all_values[[x]]), all_values[[x]])) == 0
  }, logical(1))

  return(any(!all[["all_equal"]]) || any(!all_factors))
}

##' Create a coordinate variable
##'
##' Creates a coordinate variable with associated dimensions
##' @title Create a coordinate variable
##' @param name Name of the variable
##' @param dims Dimensions of all variables
##' @param dim_factors Factors of all dimensions
##' @param index_table data.table with index columns
##' @param coord_dim Coordinate dimension
##' @param ns_dim ns dimension
##' @param time_dim time dimension
##' @param nr_column nr column
##' @param value_column value column
##' @importFrom data.table data.table
##' @importFrom reshape2 melt
##' @return a list with information on the coordinate variable
##' @keywords internal
##' @author Sebastian Funk
create_coord_var <- function(name, dims, dim_factors, coord_dim, index_table,
                             ns_dim, time_dim, nr_column, value_column) {
  coord_var <- paste("coord", name, sep = "_")
  if (length(coord_dim) > 1) {
    coord_index <- paste("index", coord_var, sep = "_")
    coord_index_values <- seq_along(coord_dim) - 1
    coord_index_dim <- list(ncdim_def(coord_index, "", coord_index_values))
    names(coord_index_dim) <- coord_index
  } else {
    coord_index_dim <- list()
  }
  for (loop_coord_dim in coord_dim) {
    dim_index <- index_table[[loop_coord_dim]]
    if (!((is.integer(dim_index) || is.numeric(dim_index)) &&
          length(setdiff(as.integer(dim_index), dim_index)) == 0 &&
          length(setdiff(seq_len(max(dim_index)), unique(dim_index))) == 0)) {
      if (any(class(dim_index) == "factor")) {
        dim_factors[[loop_coord_dim]] <- union(
          dim_factors[[loop_coord_dim]], levels(dim_index)
        )
      }
      index_table[[loop_coord_dim]] <- as.integer(factor(
        dim_index, levels = dim_factors[[loop_coord_dim]]
      )) - 1L
    } else {
      index_table[[loop_coord_dim]] <- as.integer(dim_index)
    }
  }

  coord_var_dims <- dims[sub("^coord", "nr", coord_var)]
  if (length(coord_dim) > 1) {
    sort_keys <- c(coord_dim)
    index_table_cols <- colnames(index_table)
    if (nr_column %in% index_table_cols) sort_keys <- c(nr_column, sort_keys)
    if ("ns" %in% index_table_cols) sort_keys <- c(sort_keys, "ns")
    setkeyv(index_table, sort_keys)
    id_columns <- c()
    if (nr_column %in% index_table_cols) id_columns <- c(nr_column, id_columns)
    if ("ns" %in% index_table_cols) id_columns <- c("ns", id_columns)
    if (!is.null(time_dim) && time_dim %in% colnames(index_table)) {
      id_columns <- c(time_dim, id_columns)
    }
    coord_table <- data.table(melt(
      index_table[, c(id_columns, coord_dim), with = FALSE],
      id.vars = id_columns
    ))
    sort_keys <- rev(setdiff(colnames(coord_table), "value"))
    setkeyv(coord_table, sort_keys)
    values <- coord_table[[value_column]]
    coord_var_dims <- c(coord_var_dims, list(coord_index_dim[[coord_index]]))
  } else {
    values <- index_table[[coord_dim]]
  }
  if (!is.null(ns_dim)) {
    coord_var_dims <- c(list(ns_dim), coord_var_dims)
  }
  var <- ncvar_def(coord_var, "", coord_var_dims)
  return(list(
    name = coord_var, values = values, var = var,
    dim = coord_index_dim, dim_factors = dim_factors
  ))
}

##' Get the factor levels of all character columns in data
##'
##' @param ... variable lists
##' @return a list with elements that represent the factor levels present in
##'   character columns
##' @author Sebastian Funk
get_char_levels <- function(...) {
  levels <- list()
  for (variables in list(...)) {
    ## convert character strings to factors
    data_frames <- names(variables)[
      vapply(variables, is.data.frame, logical(1))
    ]
    if (length(data_frames) > 0) {
      common <- rbindlist(variables[data_frames], fill = TRUE)
      char_cols <- colnames(common)[vapply(common, is.character, logical(1))]
      for (col in char_cols) {
        levels[[col]] <- union(levels[[col]], unique(na.omit(common[[col]])))
      }
    }
  }
  return(levels)
}

##' Convert character columns to factors in data
##'
##' @param levels factor levels, as a named list, each representing one column
##' @inheritParams bi_write
##' @return the \code{variables} argument with factorised columns
##' @author Sebastian Funk
factorise <- function(variables, levels) {
  data_frames <- names(variables)[
    vapply(variables, is.data.frame, logical(1))
  ]
  if (length(data_frames) > 0) {
    for (col in names(levels)) {
      ## convert character strings to factors
      variables[data_frames] <- lapply(variables[data_frames], function(df) {
        if (col %in% colnames(df)) {
          df[[col]] <- factor(df[[col]], levels = levels[[col]])
        }
        return(df)
      })
    }
  }
  return(variables)
}
