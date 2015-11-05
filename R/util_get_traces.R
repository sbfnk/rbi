#' @rdname get_traces
#' @name get_traces
#' @title Get the parameter traces
#' @description
#' This function takes the provided \code{bi_wrapper} which has been
#' run and returns a data frame with the parameter traces.
#' @param wrapper a \code{bi_wrapper} which has been run
#' @param ... parameters to \code{bi_read_file} (e.g., dimensions)
#' @return data frame with parameter traces; this can be fed to \code{coda} routines
#' @export
get_traces <- function(wrapper, all = FALSE, ...) {

  model <- wrapper$model
  res <- bi_read_file(wrapper$output_file_name, ...)

  if (all) {
    params <- names(res)
  } else {
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
