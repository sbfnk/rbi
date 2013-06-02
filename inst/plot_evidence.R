# plot the cumulated log evidence against time,
# ie log p(y_1, ..., y_t) against 1, ..., t
# given a vector of file names that must correspond to files with 
# a "evidence" variable in each of them, of the same length.
plot_evidence <- function(netCDFfiles){
  theme_set(theme_bw())
  allevidences <- getEvidence(netCDFfiles)
  graph <- ggplot(allevidences, aes(x = time, y = evidence, colour = run)) + geom_line()
  return(graph)
}