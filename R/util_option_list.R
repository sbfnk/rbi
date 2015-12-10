#' @rdname option_list
#' @name option_list
#' @title Convert string to option list
#' @description
#' This function is used to convert an option string into a list of options. If a list is given, it will be kept as is
#'
#' @param ... any number of strings to convert
#' @return option list
option_list <- function(...){

  options <- list()

  for (string in list(...)) {
    if (is.character(string)) {
      ## split string into options
      option_string <- strsplit(string, "--")[[1]]
      ## remove first element (as the string should start with '--')
      option_string <- option_string[-1]
      ## remove trailing blanks
      option_string <- sub("[[:space:]]*$", "", option_string)

      option_list <- lapply(option_string, function(x) {
        ## check if it has an optional argument
        opt_split <- strsplit(x, "(=|[[:space:]])[[:space:]]*")[[1]]
        if (length(opt_split) > 1) {
          ret <- opt_split[2]
          names(ret) <- opt_split[1]
        } else if (length(grep("^enable-", x)) > 0) {
          ret <- TRUE
          names(ret) <- sub("^enable-", "", x)
        } else {
          ret <- ""
          names(ret) <- x
        }
        return(ret)
      })

      option_names <- sapply(option_list, names)
      option_list <- lapply(option_list, function(x) unname(x))
      options[names] <- option_list
    } else if (is.list(string)) {
      options[names(string)] <- string
    } else {
      stop("arguments to 'option_list' must be lists or character vectors")
    }
  }


  return(options)
}
