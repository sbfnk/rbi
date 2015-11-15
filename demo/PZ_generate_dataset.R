### This demo shows how to generate data from a model using bi_generate_dataset

rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

library('ggplot2', quietly = TRUE)
library('gridExtra', quietly = TRUE)

# the PZ model file is included in RBi and can be found there:
model_file_name <- system.file(package="bi", "PZ.bi")

T <- 100
# First let's generate a dataset without specifying parameters
# so the parameters and initial conditions are drawn from the prior distribution
# which is specified in the model file.
dataset1 <- bi_generate_dataset(endtime=T, model_file_name=model_file_name,
                                working_folder=working_folder)
# Read results
output1 <- bi_read(dataset1, c("P_obs", "sigma"))

P_obs1 <- output1[["P_obs"]]$value
sigma1 <- output1[["sigma"]]

theme_set(theme_bw())
g1 <- qplot(x = seq_along(P_obs1), y = P_obs1, geom = "line") + geom_line(colour = "orange")
g1 <- g1 + xlab("time") + ylab(paste("P_obs with sigma=", round(sigma1, 2)))
# Then generate dataset with a specified set of parameter values;
# the parameters left unspecified are drawn from the prior.
dataset2 <- bi_generate_dataset(endtime=T, model_file_name=model_file_name,
                                working_folder=working_folder,
                                init = list(sigma = 0.5))
output2 <- bi_read(dataset2, c("P_obs", "sigma"))

P_obs2 <- output2[["P_obs"]]$value
sigma2 <- output2[["sigma"]]
g2 <- qplot(x = seq_along(P_obs2), y = P_obs2, geom = "line") + geom_line(colour = "orange")
g2 <- g2 + xlab("time") + ylab(paste("P_obs with sigma=", round(sigma2, 2)))
grid.arrange(g1,g2)
