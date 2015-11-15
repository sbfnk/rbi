rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

# the PZ model file is included in RBi and can be found there:
model_file_name <- system.file(package="bi", "PZ.bi")

T <- 50

init_parameters <- list(P = 2, Z = 2, mu = 0.5, sigma = 0.3)
# First let's generate a dataset from the model
synthetic_dataset <- bi_generate_dataset(endtime=T, model_file_name=model_file_name,
                                         init = init_parameters)

# Settings
bi_object <- libbi$new(client="sample", 
                       model_file_name=model_file_name,
                       working_folder = working_folder,
                       global_options=list(sampler = "smc2"))
print(bi_object)

# Once happy with the settings, launch bi.
bi_object$run(add_options = list("end-time" = T, noutputs = T, nsamples = 128, nparticles = 128, nthreads = 1), verbose = TRUE, obs = synthetic_dataset, init = init_parameters, stdoutput_file_name = tempfile(pattern="smc2output", fileext=".txt"))
# It can be a good idea to look at the result file
bi_file_summary(bi_object$result$output_file_name)
# Have a look at the posterior distribution
logweight <- bi_read(bi_object, "logweight")$value
weight <- log2normw(logweight)
mu <- bi_read(bi_object, "mu")$value
g1 <- qplot(x = mu, y = ..density.., weight = weight, geom = "histogram") + xlab(expression(mu))
sigma <- bi_read(bi_object, "sigma")$value
g2 <- qplot(x = sigma, y = ..density.., weight = weight, geom = "histogram") + xlab(expression(sigma))
grid.arrange(g1, g2)
