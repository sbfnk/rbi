rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)
  
# Settings
bi_object <- bi_wrapper$new(client = "sample", 
                        config = "posterior.conf",
                        global_options = "--sampler smc2",
                        path_to_model = "~/workspace/pz")
print(bi_object)

# Once happy with the bi_wrapper$new, launch bi.
bi_object$run(add_options=" --end-time 25 --noutputs 25 -nsamples 256 --nparticles 128 --verbose --nthreads 1",
  output_file_name = "results/launchPZ_SMC2.nc")
# It can be a good idea to look at the result file
bi_file_summary(bi_object$result$output_file_name)
# Have a look at the posterior distribution
logweight <- bi_read_var(bi_object$result$output_file_name, "logweight")
weight <- log2normw(logweight)
mu <- bi_read_var(bi_object$result$output_file_name, "mu")
g1 <- qplot(x = mu, y = ..density.., weight = weight, geom = "histogram") + xlab(expression(mu))
sigma <- bi_read_var(bi_object$result$output_file_name, "sigma")
g2 <- qplot(x = sigma, y = ..density.., weight = weight, geom = "histogram") + xlab(expression(sigma))
grid.arrange(g1, g2)