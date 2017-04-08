#' @rdname bi_read
#' @name bi_read
#' @title Bi Read
#' @description
#' This function reads all variable from a NetCDF file or the output of a
#' \code{\link{libbi}} object.
#' The file can be specified as a string to the filepath, in which
#' case a NetCDF connection is opened, or directly as a NetCDF connection.
#'
#' @param x either a path to a NetCDF file, or a NetCDF connection created using \code{nc_open}, or a \code{\link{libbi}} object from which to read the output
#' @param vars variables to read; if not given, all will be read
#' @param dims factors for dimensions
#' @param model model file or a \code{bi_model} object (if \code{x} is not a \code{libbi} object)
#' @param type vector of types of variable to read (out of "param", "state", "noise", "obs"). This needs 'x' to be a \code{\link{libbi}} object or \code{model} to be specified
#' @param missval.threshold upper threshold for the likelihood
#' @param coord_name name of coord dimension (if any)
#' @param vector if TRUE, will return results as vectors, not data.frames
#' @param thin thinning (keep only 1/thin of samples)
#' @param verbose if TRUE, will print variables as they are read
#' @param clear_cache if TRUE, will clear the cache and re-read the file even if cached data exists
#' @param init.to.param logical; if TRUE, convert states to initial values
#' @return list of results
#' @importFrom ncdf4 nc_close ncvar_get
#' @importFrom data.table setkeyv setnames setDF is.data.table
#' @importFrom reshape2 melt
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @examples
#' example_output_file <- system.file(package="rbi", "example_output.nc")
#' d <- bi_read(example_output_file)
#' @export
bi_read <- function(x, vars, dims, model, type, missval.threshold, coord_name, vector, thin, verbose, clear_cache, init.to.param=FALSE)
{
  nc <- bi_open(x)
  res <- list()

  thin <-
    as.integer(ifelse(missing(thin),
                      ifelse("libbi" %in% class(x), x$thin, 1), thin))

  if ("libbi" %in% class(x) && !is.null(x$dims)) {
    if (missing(model)) {
      model <- x$model
    } else {
      stop("'model' should not be given if 'data' is a 'libbi' object'.")
    }
    if (missing(dims) || is.null(dims)) {
      dims <- x$dims
    } else {
      warning("Given 'dims' will overwrite dimensions in passed libbi object")
    }
  }

  all_nc_var_names <- unname(vapply(nc[["var"]], function(y) { y[["name"]] }, ""))

  time_coord_names <- c()
  nc_var_names <- list()
  ## special variables
  arg_names <- names(as.list(match.call()[-1]))
  for (var_type in c("coord", "time")) {
    time_coord_names[var_type] <- ifelse(paste(var_type, "name", sep = "_") %in% arg_names, get(paste(var_type, "name", sep = "_")), var_type)
    nc_var_names[[var_type]] <- grep(paste0("^", var_type), all_nc_var_names, value = TRUE)
  }
  forbidden_names <-
    intersect(vapply(nc[["dim"]], function(y) {y[["name"]]}, ""),
              time_coord_names)
  if (length(forbidden_names) > 0) {
    stop("Can't have a dimension called ", paste(forbidden_names, sep = ", "), ".")
  }
  nc_var_names[["other"]] <- setdiff(all_nc_var_names, unlist(nc_var_names))

  if (!missing(vars) && !missing(type)) {
    stop("Only one of 'vars' and 'type' can be given.")
  }

  if (!missing(type)) {
    vars <- var_names(model, type, opt=TRUE)
    vars <- grep("has_output[^=]*=[^[0-1]*0", vars, value=TRUE, invert=TRUE)
  }

  if (!missing(vars)) {
    missing_vars <- setdiff(vars, nc_var_names[["other"]])
    if (length(missing_vars) > 0) {
      if (all(vars %in% c(nc_var_names[["coord"]], nc_var_names[["time"]]))) {
        ## we're actually reading the time/coord vars
        nc_var_names[["coord"]] <- character(0)
        nc_var_names[["time"]] <- character(0)
        nc_var_names[["other"]] <- vars
      } else {
        warning("Variable(s) ", missing_vars, " not found")
      }
    }
    nc_var_names[["other"]] <- intersect(vars, nc_var_names[["other"]])
  }

  ## read dimensions
  var_dims <- list()
  for (var_type in names(nc_var_names)) {
    var_dims[[var_type]] <- list()
    for (var_name in nc_var_names[[var_type]]) {
      var <- nc[["var"]][[var_name]]
      dim_names <- vapply(var$dim, function(y) { y$name }, "")
      var_dims[[var_type]][[var_name]] <- dim_names[nchar(dim_names) > 0]
    }
  }

  res <- base::vector("list", length(nc_var_names[["other"]]))
  names(res) <- nc_var_names[["other"]]

  ## associate time and coord variables
  ## read variables
  if ("libbi" %in% class(x) && x$use_cache) {
    if (!missing(clear_cache) && clear_cache) {
      x$.cache$data <- NULL
      x$.cache$thin <- NULL
    }
    cached_other <- vapply(nc_var_names[["other"]], function(y) {
      return(y %in% names(x$.cache$data) && thin == x$.cache$thin[y])
    }, TRUE)
  } else {
    cached_other <- rep(FALSE, length(nc_var_names[["other"]]))
  }
  for (var_name in nc_var_names[["other"]][cached_other]) {
    if (!missing(verbose) && verbose) {
      message(date(), " ", var_name, " (cached)")
    }
    res[[var_name]] <- x$.cache$data[[var_name]]
  }
  for (var_name in nc_var_names[["other"]][!cached_other]) {
    if (!missing(verbose) && verbose) {
      message(date(), " ", var_name)
    }
    if (missing(vars) || var_name %in% vars) {
      dim_names <- var_dims[["other"]][[var_name]]
      dim_lengths <- vapply(seq_along(dim_names), function(y)
      {
        nc[["var"]][[var_name]][["dim"]][[y]][["len"]]
      }, 0)
      names(dim_lengths) <- dim_names
      if ("np" %in% dim_names && thin > 1) {
        np_indices <- seq(1, dim_lengths["np"], by = thin)
        dim_lengths["np"] <- length(np_indices)

        all_values <- array(dim = dim_lengths)

        if (!missing(verbose) && verbose) pb <- txtProgressBar(min = 0, max = length(np_indices), char = ".", style = 1)

        for (i in seq_along(np_indices))
        {
          dim_list <- lapply(dim_lengths, seq_len)
          dim_list[["np"]] <- i
          start_vec <- dim_lengths
          start_vec[] <- 1
          start_vec["np"] <- np_indices[i]
          count_vec <- dim_lengths
          count_vec["np"] <- 1
          all_values <-
            do.call('[<-',
                    c(list(all_values),
                      unname(dim_list),
                      list(ncvar_get(nc, var_name,
                                     start = start_vec,
                                     count = count_vec))))
          if (!missing(verbose) && verbose) setTxtProgressBar(pb, i)
        }
        if (!missing(verbose) && verbose) close(pb)
      } else {
        all_values <- ncvar_get(nc, var_name)
      }

      ## check for any auxiliary dimensions (linking with time or coord variables)
      auxiliary_dims <- unname(unlist(var_dims[c("coord", "time")]))
      dim_names <- dim_names[dim_lengths > 1 | dim_names %in% auxiliary_dims]

      if (any(duplicated(dim_names))) {
        duplicated_dim_names <- dim_names[duplicated(dim_names)]
        for (dup_dim in duplicated_dim_names) {
          dim_names[dim_names == dup_dim] <-
            paste(dup_dim, seq_along(dim_names[dim_names == dup_dim]), sep = ".")
        }
      }

      ## preserve auxiliary dimensions of length 1
      n_one_dims <- dim_names[dim_lengths[dim_names] == 1]
      if (length(n_one_dims) > 0) {
        all_values <- array(all_values, dim=c(dim(all_values), rep(1, length(n_one_dims))))
      }

      if (prod(dim(all_values)) + length(n_one_dims) > 1) {

        mav <- data.table::data.table(reshape2::melt(all_values, varnames = dim_names))
        ## remove any extraneous dimensions from melting
        mav <- mav[, c(dim_names, "value"), with=F]

        ## find matching time and coord variables
        all_matching_dims <- c()
        for (var_type in names(time_coord_names)) {
          matching_dims <- unlist(var_dims[[var_type]][unlist(var_dims[[var_type]]) %in% dim_names])
          matching_vars <- names(matching_dims)
          all_matching_dims <- union(all_matching_dims, matching_dims)
          if (length(matching_vars) == 1)  {
            merge_values <- ncvar_get(nc, matching_vars)
            mav_merge <- data.table::data.table(reshape2::melt(merge_values, varnames = rev(matching_dims), value.name = time_coord_names[var_type]))
            mav <- merge(mav_merge, mav, by = unname(matching_dims))
          } else if (length(matching_vars) > 1) {
            stop("Found multiple matching time variables for ", var_name, ": ", matching_vars)
          }
        }

        for (var in all_matching_dims) {
          mav[[var]] <- NULL
        }
        table_order <- c(setdiff(colnames(mav), c(time_coord_names, "value")),
                         intersect(colnames(mav), time_coord_names),
                         "value")

        mav <- mav[, table_order, with = FALSE]

        ## reorder duplicates
        cols <- setdiff(colnames(mav), "value")
        if (length(cols) > 0) setkeyv(mav, cols)
        rownames(mav) <- seq_len(nrow(mav))

        if (!missing(dims) && !is.null(dims) && length(dims) == 1 && "coord" %in% colnames(mav)) {
          setnames(mav, "coord", names(dims))
        }

        for (col in colnames(mav)) {
          if (!missing(dims) && !is.null(dims) && col %in% names(dims)) {
            mav[[col]] <- factor(mav[[col]], labels = dims[[col]])
          } else if (!(col %in% c("value", time_coord_names[c("time", "coord")]))) {
            mav[[col]] <- mav[[col]] - 1
          }
        }
      } else {
        ## dimensionless values
        mav <- all_values
      }

      if (!missing(missval.threshold)) {
        if (data.table::is.data.table(mav)) {
          missing.values <- which(mav$value > missval.threshold)
          if (length(missing.values) > 0) {
            mav[missing.values, ]$value <- NA_real_
          }
        } else {
          mav[mav > missval.threshold] <- NA_real_
        }
      }

      if (!missing(vector) && vector) {
        res[[var_name]] <- mav$value
      } else {
        if (data.table::is.data.table(mav)){
          res[[var_name]] <- setDF(mav)
        } else {
          res[[var_name]] <- mav
        }
      }
    }
  }

  if (typeof(x) %in% c("character", "libbi")) nc_close(nc)

  if ("libbi" %in% class(x) && x$use_cache) {
    if (!("data" %in% names(x$.cache))) {
      x$.cache$data <- list()
    }
    if (!("thin" %in% names(x$.cache))) {
      x$.cache$thin <- list()
    }
    for (name in nc_var_names[["other"]][!cached_other]) {
      x$.cache$data[[name]] <- res[[name]]
      x$.cache$thin[[name]] <- thin
    }
  }

  if (init.to.param) {
    res <- lapply(res, function(x) {
      if (is.data.frame(x) && "time" %in% colnames(x)) {
        min_time <- min(x$time)
        x <- x[x$time == min_time, ]
        x$time <- NULL
      }
      x
    })
  }

  return(res)
}
