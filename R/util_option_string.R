#' @rdname option_string
#' @name option_string
#' @title Convert Options
#' @description
#' This function is used to convert a list of options into an options string. If a string is given,  it will be taken as such.
#'
#' @param ... any number of lists of options, or strings (which will be left unmodified). If lists are given, later arguments will override earlier ones
#
option_string <- function(...){
  list_options <- list()
  string <- ""

  for (option in list(...)) {
    if (is.list(option)) {
      list_options[names(option)] <- option
    } else {
      stop("arguments to 'option_string' must be lists or character vectors")
    }
  }

  string <- lapply(names(list_options),
                   function(option) {
                       if (is.logical(list_options[[option]])) {
                           if (option == "verbose") {
                               return("--verbose")
                           } else {
                               if (list_options[[option]] == TRUE)
                                   return(paste0("--enable-", option))
                               else
                                   return(paste0("--disable-", option))
                           }
                       } else if (option == "dry") {
                           return(paste0("--dry-", list_options[[option]]))
                       } else if (option %in% c("with", "without")) {
                           return(paste0("--", option, "-", list_options[[option]]))
                       } else {
                           ret <- c(paste0("--", option))
                           value <- list_options[[option]]
                           if (value != "") {
                               ret <- c(ret, format(list_options[[option]],
                                                    scientific = FALSE))
                           }
                           return(ret)
                       }
                   })

  return(unlist(string))
}
