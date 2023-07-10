### This demo shows how to generate data from a model using generate_dataset

# the PZ model file is included in rbi and can be found there:
model_file_name <- system.file(package = "rbi", "PZ.bi")

# assign model variable
pz <- bi_model(model_file_name)
# look at the model
pz

tf <- 100
# First let's generate a dataset without specifying parameters
# so the parameters and initial conditions are drawn from the prior distribution
# which is specified in the model file.
# if libbi throws an error here, try passing the "working_folder" argument to
# 'generate_dataset' and look for the issue in the ".PZ" subdirectory
dataset1 <- generate_dataset(pz, end_time = tf, noutputs = tf)
# Read results
output1 <- bi_read(dataset1, c("P_obs", "sigma"))

p_obs1 <- output1[["P_obs"]]$value
sigma1 <- output1[["sigma"]]

par(mfrow = c(2, 1))
plot(
  seq_along(p_obs1), p_obs1,
  type = "l", col = "orange", xlab = "time",
  ylab = paste("P_obs with sigma=", round(sigma1, 2))
)

# Then generate dataset with a specified set of parameter values;
# the parameters left unspecified are drawn from the prior.
dataset2 <- generate_dataset(
  pz,
  end_time = tf, noutputs = tf, init = list(sigma = 0.5)
)
output2 <- bi_read(dataset2, c("P_obs", "sigma"))

p_obs2 <- output2[["P_obs"]]$value
sigma2 <- output2[["sigma"]]

plot(
  seq_along(p_obs2), p_obs2,
  type = "l", col = "orange", xlab = "time",
  ylab = paste("P_obs with sigma=", round(sigma2, 2))
)
