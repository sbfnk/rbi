#' @rdname output_to_proposal
#' @name output_to_proposal
#' @title Construct a proposal from run results
#' @description
#' This function takes the provided \code{bi_wrapper} which has been
#' run and returns a new model which has the proposal constructed from
#' the sample mean and standard deviation.
#' @param wrapper a \code{bi_wrapper} which has been run
#' @param scale a factor by which to scale all the standard deviations
#' @return the updated bi model
#' @export
output_to_proposal <- function(wrapper, scale) {

  model <- wrapper$model
  params <- model$get_vars("param")
  res <- bi_read_file(wrapper$output_file_name, variables = params)

  if (missing(scale)) {
    scale_string <- ""
  } else {
    scale_string <- paste0(scale, " * ")
  }

  param_sd <- sapply(params, function(p) {sd(res[[p]]$value)})
  param_bounds <- model$get_block("parameter")

  proposal_lines <- unname(sapply(names(param_sd), function(param) {
    bounds_line <-
      grep(paste0("^[[:space:]]*", param, "[^~]*~"), param_bounds,
           value = TRUE)
    bounds_string <- sub("^.*uniform\\(([^\\)]+)\\).*$", "\\1",
                         bounds_line)
    bounds <- strsplit(bounds_string, split = ",")[[1]]
    param_string <- sub(paste0("^[[:space:]]*", param, "([^~]*)~.*$"),
                        paste0(param, "\\1"), bounds_line)
    param_string <- sub("[[:space:]]+$", "", param_string)

    paste0(param_string, " ~ truncated_normal(",
           "mean = ", param_string,
           ", std = ", scale_string, param_sd[param], ", ",
           paste(c("lower", "upper"), "=", bounds, sep = " ", collapse = ", "),
           ")")
  }))

  model$add_block(name = "proposal_parameter",
                  lines = proposal_lines)

  return(model)
}
