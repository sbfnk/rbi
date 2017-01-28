### This demo shows how to generate data from a model using bi_generate_dataset

# the PZ model file is included in rbi and can be found there:
model_file_name <- system.file(package="rbi", "PZ.bi")

# assign model variable
PZ <- bi_model(model_file_name)
# look at the model
PZ

T <- 100
# First let's generate a dataset without specifying parameters
# so the parameters and initial conditions are drawn from the prior distribution
# which is specified in the model file.
# if libbi throws an error here, try passing the "working_folder" argument to
# 'bi_generate_dataset' and look for the issue in the ".PZ" subdirectory
dataset1 <- bi_generate_dataset(model=PZ, end_time=T, noutputs=T)
# Read results
output1 <- bi_read(dataset1, c("P_obs", "sigma"))

P_obs1 <- output1[["P_obs"]]$value
sigma1 <- output1[["sigma"]]

par(mfrow = c(2, 1))
plot(seq_along(P_obs1), P_obs1, type = "l", col = "orange", xlab = "time",
     ylab = paste("P_obs with sigma=", round(sigma1, 2)))

# Then generate dataset with a specified set of parameter values;
# the parameters left unspecified are drawn from the prior.
dataset2 <- bi_generate_dataset(end_time=T, model=PZ, noutputs=T, 
                                init = list(sigma = 0.5))
output2 <- bi_read(dataset2, c("P_obs", "sigma"))

P_obs2 <- output2[["P_obs"]]$value
sigma2 <- output2[["sigma"]]

plot(seq_along(P_obs2), P_obs2, type = "l", col = "orange", xlab = "time",
     ylab = paste("P_obs with sigma=", round(sigma2, 2)))

## alternatively, one could plot using the 'rbi.helpers package'
##
## library('rbi.helpers')
##
## plot(dataset1)
## plot(dataset2)
##
## p1 <- plot(dataset1, state = "P_obs", color = "orange")
## p2 <- plot(dataset2, state = "P_obs", color = "orange")
## g1 <- p1$states + xlab("time") + ylab(paste("P_obs with sigma=", round(sigma1, 2)))
## g2 <- p2$states + xlab("time") + ylab(paste("P_obs with sigma=", round(sigma2, 2)))
## grid.arrange(g1,g2)
