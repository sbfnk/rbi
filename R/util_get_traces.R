#' @rdname get_traces
#' @name get_traces
#' @title Get the parameter traces
#' @description
#' This function takes the provided \code{bi_wrapper} which has been
#' run and returns a data frame with the parameter traces.
#' @param run a \code{bi_wrapper} which has been run, or a file (in
#'   which case either 'all' must be TRUE or a model given 
#' @param all whether all variables in the run file should be
#'   considered (otherwise, just parameters)
#' @param model a model to get the parameter names from; not needed if
#'   'run' is given as a \code{bi_wrapper} object or 'all' is set to
#'   TRUE 
#' @param ... parameters to \code{bi_read} (e.g., dimensions)
#' @return data frame with parameter traces; this can be fed to \code{coda} routines
#' @export
get_traces <- function(run, all = FALSE, model, ...) {

  if (class(run) == "libbi") {
    res <- bi_read_file(run$output_file_name, ...)
    if (missing(model)) {
      model <- run$model
    } else {
      warning("Given model will overwrite model contained in given 'run'.")
    }
  } else {
    res <- bi_read_file(run, ...)
    if (missing(model)) {
      model <- NULL
    }
  }
  
  if (all) {
    params <- names(res)
  } else {
    if (is.null(model)) {
      stop("Either 'all' must be set to TRUE, or a model given (via the 'run' or 'model' options)")
    }
    params <- model$get_vars("param")
  }

  wide_list <- lapply(params, function(param) {
    extra.dims <- setdiff(colnames(res[[param]]), c("np", "param", "value"))
    if (length(extra.dims) > 0) {
      df <- dcast(res[[param]],
                  as.formula(paste("np", paste(extra.dims, collapse = "+"), sep = "~")))
      names(df)[-1] <- paste(param, names(df)[-1], sep = ".")
    } else {
      df <- res[[param]]
      names(df)[which(names(df) == "value")] <- param
    }
    df[, -1, drop = FALSE]
  })

  return(do.call(cbind, wide_list))
}
