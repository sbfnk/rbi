#' @export
run <- function(x, ...) UseMethod("run")

default_sample <- sample
#' @export
sample <- function(x, ...) UseMethod("sample")
#' @export
sample.default <- function(x, ...){
  default_sample(x, ...)
}

default_filter <- filter
#' @export
filter <- function(x, ...) UseMethod("filter")
#' @export
filter.default <- function(x, ...){
  default_filter(x, ...)
}

default_optimise <- optimise
#' @export
optimise <- function(x, ...) UseMethod("optimise")
#' @export
optimise.default <- function(x, ...){
  default_optimise(x, ...)
}

#' @export
rewrite <- function(x, ...) UseMethod("rewrite")

#' @export
add_output <- function(x, ...) UseMethod("add_output")

default_saveRDS <- saveRDS
#' @export
saveRDS <- function(x, ...) UseMethod("saveRDS")
#' @export
saveRDS.default <- function(x, ...){
  default_saveRDS(x, ...)
}

default_readRDS <- readRDS
#' @export
readRDS <- function(x, ...) UseMethod("readRDS")
#' @export
readRDS.default <- function(x, ...){
  default_readRDS(x, ...)
}

default_fix <- fix
#' @export
fix <- function(x, ...) UseMethod("fix")
#' @export
fix.default <- function(x, ...){
  default_fix(x, ...)
}

#' @export
insert <- function(x, ...) UseMethod("insert")

default_remove <- remove
#' @export
remove <- function(x, ...) UseMethod("remove")
#' @export
remove.default <- function(x, ...){
  default_remove(x, ...)
}

#' @export
write_file <- function(x, ...) UseMethod("write_file")

#' @export
get_block <- function(x, ...) UseMethod("get_block")



