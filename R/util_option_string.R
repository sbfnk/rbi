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
    if (is.character(option)) {
      string_options <- option_list(option)
      list_options[names(string_options)] <- string_options
    } else if (is.list(option)) {
      list_options[names(option)] <- option
    } else {
      stop("arguments to 'option_string' must be lists or character vectors")
    }
  }

  string <- paste(paste(sapply(names(list_options),
                         function(option) {
                           if (is.logical(list_options[[option]])) {
                             if (option == "verbose") {
                                 paste0("--verbose")
                             } else {
                               if (list_options[[option]] == TRUE)
                                 paste0("--enable-", option)
                               else
                                 paste0("--disable-", option)
                             }
                           } else if (option == "dry") {
                             paste0("--dry-", list_options[[option]])
                           } else if (option %in% c("with", "without")) {
                             paste("--", option, list_options[[option]],
                                   sep="-", collapse=" ")
                           } else {
                             paste0("--", option, " ",
                                    format(list_options[[option]],
                                           scientific = FALSE))
                           }
                         }
                         ), collapse = " "), string)


  return(string)
}
