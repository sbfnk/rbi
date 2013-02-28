rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

# Settings
settings <- bi::settings(mode = "filter", configfile = "filter.conf",
pathModel = "/home/pierre/workspace/pz/",
pathBi = "/home/pierre/workspace/bi/script/",
args = "--disable-assert --enable-sse --enable-diagnostics --enable-timing --threads 1")
print(settings)
verbose = FALSE

NCDFfile <- "/home/pierre/test.nc"
StdOutputfilename <- "/home/pierre/diagnostics.txt"

bi::launcher(settings, 
   args= paste(" -T 1000 -P 1024 --output-file", NCDFfile, "--resampler multinomial "),
   saveStdOutputFile = StdOutputfilename)

lines <- readLines(StdOutputfilename)

library(stringr)
extractSlots <- function(line){
  splitline <- str_split(line, "[[:space:]]")[[1]]
  if (splitline[1] == "AncestryCache:"){
    slot <- as.numeric(splitline[2])
  } else {
    slot <- NA
  }
  return(slot)
}
slotnumbers <- sapply(X=lines, FUN=extractSlots)
slotnumbers <- as.numeric(na.omit(as.numeric(slotnumbers)))
qplot(x = 1:length(slotnumbers), y = slotnumbers - 1:length(slotnumbers), geom = "line")
