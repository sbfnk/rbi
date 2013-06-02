# This function is used to convert relative file paths to absolute file paths
# without checking if the file exists as tools::file_as_absolute_path does
absolute_path <- function(filename, dirname){
  if (missing(dirname)){
    dirname <- ""
  }
  if (stringr::str_sub(filename, 1, 1) == "/"){
    #the filename is already absolute
    result <- normalizePath(filename, "/", FALSE)
  } else {
    if (nchar(dirname) == 0){
      result <- normalizePath(filename, "/", FALSE)
    } else {
      result <- normalizePath(paste(dirname, filename, sep = "/"), "/", FALSE)
    }
  }
  return(result)
}
