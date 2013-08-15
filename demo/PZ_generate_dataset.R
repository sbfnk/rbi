### This demo shows how to generate data from a model using bi_generate_dataset

rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

# first retrieve a simple model such as the PZ model
# if you don't have it already:
# git copy https://github.com/lawmurray/PZ.git
path_to_model <- "~/workspace/pz/"
modelfile <- "PZ.bi"

T <- 100
# First let's generate a dataset without specifying parameters
# so the parameters and initial conditions are drawn from the prior distribution
# which is specified in the model file.
dataset1 <- bi_generate_dataset(endtime=T, modelfile=modelfile,
                                path_to_model=path_to_model)
# Let's look inside
bi_file_summary(dataset1)

P_obs1 <- bi_read_var(dataset1, "P_obs")
sigma1 <- bi_read_var(dataset1, "sigma")
theme_set(theme_bw())
g1 <- qplot(x = seq_along(P_obs1), y = P_obs1, geom = "line") + geom_line(colour = "orange")
g1 <- g1 + xlab("time") + ylab(paste("P_obs with sigma=", round(sigma1, 2)))
print(g1)
# Then generate dataset with a specified set of parameter values;
# the parameters left unspecified are drawn from the prior.
dataset2 <- bi_generate_dataset(endtime=T, modelfile=modelfile, path_to_model=path_to_model,
                                parameters = list(sigma = 0.5))
P_obs2 <- bi_read_var(dataset2, "P_obs")
sigma2 <- bi_read_var(dataset2, "sigma")
g2 <- qplot(x = seq_along(P_obs2), y = P_obs2, geom = "line") + geom_line(colour = "orange")
g2 <- g2 + xlab("time") + ylab(paste("P_obs with sigma=", round(sigma2, 2)))
grid.arrange(g1,g2)
