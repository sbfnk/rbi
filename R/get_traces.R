#' @rdname get_traces
#' @name get_traces
#' @title Get the parameter traces
#' @description This function takes the provided \code{\link{libbi}} object
#'     which has been run and returns a data frame with the parameter
#'     traces.
#' @param x a \code{\link{libbi}} object which has been run, or a
#'     list of data frames containing parameter traces (as returned by
#'     from \code{bi_read}); if it is not a \code{\link{libbi}}
#'     object, either 'all' must be TRUE or a model given
#' @param all whether all variables in the run file should be
#'     considered (otherwise, just parameters)
#' @param model a model to get the parameter names from; not needed if
#'     'run' is given as a \code{\link{libbi}} object or 'all' is set
#'     to TRUE
#' @param burnin proportion of iterations to discard as burn-in
#' @param ... parameters to \code{bi_read} (e.g., dimensions)
#' @return data frame with parameter traces; this can be fed to
#'     \code{coda} routines
#' @importFrom data.table data.table dcast
#' @importFrom stats as.formula
#' @export
get_traces <- function(x, model, burnin, all = FALSE, ...) {

  read_options <- list(x = x, ...)

  if ("libbi" %in% class(x)) {
    if (missing(model)) {
      model <- x$model
    } else {
      warning("Given model will overwrite model contained in given libbi object.")
    }
  }

  if (!all) {
    if (missing(model)) {
      stop("Either 'all' must be set to TRUE, or a model given (implicitly via the 'x' or explicitly via 'model' options)")
    } else {
      if (is.character(model)) model <- bi_model(model)
      read_options <- c(read_options, list(type = "param", model = model))
    }
  }

  if (("libbi" %in% class(x)) || ("character" %in% class(x))) {
      res <- do.call(bi_read, read_options)
  } else if ("list" %in% class(x)) {
      if (all) {
        res <- x
      } else {
        res <- x[intersect(names(x), var_names(model, "param"))]
      }
  } else {
      stop("'x' must be a 'libbi' object or a file name or a list of data frames.")
  }

  if (!missing(burnin) && !(burnin > 0 && burnin < 1)) {
    stop("'burnin' must be between 0 and 1.")
  }

  wide_list <- lapply(names(res), function(param) {
    extra.dims <- setdiff(colnames(res[[param]]), c("np", "param", "value"))
    if (length(extra.dims) > 0) {
      df <-
        dcast(data.table(res[[param]]),
              as.formula(paste("np", paste(extra.dims, collapse = "+"),
                               sep = "~")), value.var = "value")
      names(df)[-1] <- paste(param, names(df)[-1], sep = ".")
    } else {
      if (prod(dim(res[[param]])) == 1) {
        ## just a single value
        df <- data.frame(np = 0, value = res[[param]])
      } else {
        df <- res[[param]]
      }
      names(df)[which(names(df) == "value")] <- param
    }
    df[, -1, drop = FALSE]
  })

  ret <- do.call(cbind, wide_list)

  if (!missing(burnin)) {
    ret <- ret[-seq_len(floor(burnin * nrow(ret))), ]
  }

  return(ret)
}
