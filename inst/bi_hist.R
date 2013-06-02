bi_hist <- function(file, name, coord, ps, t, bins){
  if (missing(coord))
    coord <- c()
  if (missing(ps))
    ps <- c()
  if (missing(t))
    t <- c()
  if (missing(bins))
    bins = 20
#   if (missing(col))
  col = "blue"
  values = bi_read_var(file, name, coord, ps, ts)
  global_attributes = nc_get_attributes(ncfile)
  if (global_attributes$libbi_schema == "ParticleMCMC"){
    binwidth = (max(values) - min(values)) / bins
    g <- qplot(x = values, geom = "blank") + geom_histogram(fill = col,
                                                            binwidth = binwidth)
    print(g)
  }
}
