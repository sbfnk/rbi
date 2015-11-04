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

  if (!wrapper$run_flag) {
    stop("The model should be run first")
  }

  model <- wrapper$model
  params <- model$get_vars("param")
  res <- bi_read_file(wrapper$output_file_name, variables = params)

  if (missing(scale)) {
    scale_string <- ""
  } else {
    scale_string <- paste0(scale, " * ")
  }

  param_sd <- sapply(params, function(p) {
      ifelse(length(res[[p]]) == 1, 0, sd(res[[p]]$value))
  })
  
  param_bounds <- model$get_block("parameter")

  proposal_lines <- unname(sapply(names(param_sd)[param_sd > 0], function(param) {
    bounds_line <-
      grep(paste0("^[[:space:]]*", param, "[[[:space:]][^~]*~"), param_bounds,
           value = TRUE)
    param_string <- sub(paste0("^[[:space:]]*", param, "([^~]*)~.*$"),
                          paste0(param, "\\1"), bounds_line)
    param_string <- sub("[[:space:]]+$", "", param_string)
      
    param_bounds_string <-
      sub("^.*(uniform|truncated_gaussian|truncated_normal)\\(([^\\)]+)\\).*$",
          "\\1|\\2", bounds_line)

    args <- strsplit(param_bounds_string, split = "\\|")

    dist <- args[[1]][1]
    bounds_string <- args[[1]][2]

    if (bounds_string == bounds_line) {
      paste0(param_string, " ~ gaussian(",
             "mean = ", param_string,
             ", std = ", ")")
    } else {
      bounds <- c(lower = NA, upper = NA)
      
      split_bounds <- strsplit(bounds_string, split = ",")[[1]]
      for (bound in c("lower", "upper")) {
        named <- grep(paste0(bound, "[[:space:]]*="), split_bounds)
        if (length(named) > 0) {
          bounds[bound] <- split_bounds[named]
          split_bounds <- split_bounds[-named]
        }
      }

      if (any(is.na(bounds))) {
        if (length(grep("^truncated", dist)) > 0) {
          named_other <- grep("(mean|std)", split_bounds)
          if (length(named_other) > 0) {
            split_bounds <- split_bounds[-named_other]
          }
          if (length(named_other) < 2) {
            split_bounds <- split_bounds[-seq_len(2 - length(named_other))]
          }
        }
      }

      for (split_bound in split_bounds) {
        bounds[which(is.na(bounds))][1] <- split_bound
      }

      bounds <- gsub("(lower|upper)[[:space:]]*=[[:space::]]*", "", bounds)
      bounds <- as.numeric(bounds)
      bounds <- bounds[!is.na(bounds)]

      paste0(param_string, " ~ truncated_gaussian(",
             "mean = ", param_string,
             ", std = ", scale_string, param_sd[param],
             ifelse(length(bounds) > 0,
                    paste0(", ", paste(names(bounds), "=", bounds,
                                       sep = " ", collapse = ", "),
                           ")"),
                    ")"))
    }
  }))

  model$add_block(name = "proposal_parameter",
                  lines = proposal_lines)

  return(model)
}
