#' @rdname option_list
#' @name option_list
#' @title Convert string to option list
#' @description
#' This function is used to convert an option string into a list of options. If a list is given, it will be kept as is
#'
#' @param ... any number of strings to convert
#' @return a list of options and values
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

      string_list <- lapply(option_string, function(x) {
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

      option_names <- gsub("_", "-", vapply(string_list, names, ""))
      string_list <- lapply(string_list, function(x) unname(x))

      options[option_names] <- string_list
    } else if (is.list(string)) {
      names(string) <- gsub("_", "-", names(string))
      ## identify within-option conflicts
      conflicting <- intersect(string[["with"]], string[["without"]])
      if (length(conflicting) > 0) {
        stop("Option given as 'with' and 'without': ",
             paste(conflicting, collapse=", "))
      }
      if ("with" %in% names(string)) {
        with_list <- as.list(rep("", length(string[["with"]])))
        names(with_list) <- paste("with", string[["with"]], sep="-")
        ## remove existing 'without' options of the same name
        without_options <-
          sub("^without-", "", grep("^without-", names(options), value=TRUE))
        existing <- intersect(without_options, string[["with"]])
        options[paste("without", existing, sep="-")] <- NULL
        ## add as strings
        string[["with"]] <- NULL
        string <- c(string, with_list)
      }
      if ("without" %in% names(string)) {
        without_list <- as.list(rep("", length(string[["without"]])))
        names(without_list) <- paste("without", string[["without"]], sep="-")
        ## remove existing 'with' options of the same name
        with_options <- sub("^with-", "", grep("^with-", names(options), value=TRUE))
        existing <- intersect(with_options, string[["without"]])
        options[paste("with", existing, sep="-")] <- NULL
        ## add as strings
        string[["without"]] <- NULL
        string <- c(string, without_list)
      }
      if ("dry" %in% names(string)) {
        dry_list <- as.list(rep("", length(string[["dry"]])))
        names(dry_list) <- paste("dry", string[["dry"]], sep="-")
        string[["dry"]] <- NULL
        string <- c(string, dry_list)
      }
      for (name in names(string)) options[name] <- string[[name]]
    } else {
      stop("arguments to 'option_list' must be lists or character vectors")
    }
  }

  return(options)
}

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

  for (option in list(...)) list_options[names(option)] <- option

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
                       } else {
                           ret <- c(paste0("--", option))
                           value <- list_options[[option]]
                           if (length(value) == 0 || value != "") {
                               ret <- c(ret, format(list_options[[option]],
                                                    scientific = FALSE))
                           }
                           return(ret)
                       }
                   })

  return(unlist(string))
}
