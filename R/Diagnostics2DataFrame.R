Diagnostics2DataFrame <- function(txtfile){
  library(stringr)
  lines <- readLines(txtfile)
  slots <- c();   nodes <- c() 
  free <- c();   fragmentation <- c()
  microseconds <- c()
  for (linenumber in 1:length(lines)){
    splitline <- str_split(lines[linenumber], "[[:space:]]")[[1]]
    if (splitline[1] == "AncestryCache:"){
      slots <- c(slots, as.numeric(splitline[2]))
      nodes <- c(nodes, as.numeric(splitline[4]))
      free <- c(free, as.numeric(splitline[6]))
      fragmentation <- c(fragmentation, as.numeric(splitline[9]))
      microseconds <- c(microseconds, as.numeric(splitline[11]))
    } 
  }
  dataframe <- data.frame(cbind(slots, nodes, free, fragmentation, microseconds))
  names(dataframe) <- c("slots", "nodes", "free", "fragmentation", "microseconds")
  dataframe$iteration <- 1:length(slots)
  return(dataframe)
}