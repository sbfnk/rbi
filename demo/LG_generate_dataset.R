### This demo shows how to generate data from a model
### using bi_generate_dataset

rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

T <- 1000
# First generate dataset without specifying parameters
# so the parameters and initial conditions are drawn from the prior distribution.
dataset1 <- bi_generate_dataset(endtime=T, modelfile="LG.bi", path_to_model="~/workspace/lg/")

bi_file_summary(dataset1)
Y1 <- bi_read_var(dataset1, "Y")
X1 <- bi_read_var(dataset1, "X")
D1 <- bi_read_var(dataset1, "D")
g1 <- qplot(x = seq_along(Y1), y = Y1, geom = "line") +
  geom_line(aes(y=X1), col = "red") + xlab("time") + ylab(paste("Y with D=", round(D1, 2)))
# Then generate dataset with a specified set of parameter values;
# the parameters left unspecified are drawn from the prior.
dataset2 <- bi_generate_dataset(endtime=T, modelfile="LG.bi", path_to_model="~/workspace/lg/",
                                parameters = list(D = 0.5, X = 0))
Y2 <- bi_read_var(dataset2, "Y")
X2 <- bi_read_var(dataset2, "X")
D2 <- bi_read_var(dataset2, "D")
g2 <- qplot(x = seq_along(Y2), y = Y2, geom = "line") +
  geom_line(aes(y=X2), col = "red") + xlab("time") + ylab(paste("Y with D=", round(D2, 2)))
grid.arrange(g1,g2)