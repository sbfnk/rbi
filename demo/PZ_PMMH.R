rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

# Settings
bi_object <- bi_wrapper$new(client = "sample", 
                        config = "posterior.conf",
                        path_to_model = "~/workspace/pz")

print(bi_object)

# Once happy with the settings, launch bi.
bi_object$run(add_options = " --end-time 50 -nsamples 50 --nparticles 128 --verbose --nthreads 1",
 output_file_name = "results/launchPZ_PMMH.nc")
# It can be a good idea to look at the result file
bi_file_summary(bi_object$result$output_file_name)
# Have a look at the posterior distribution
mu <- bi_read_var(bi_object$result$output_file_name, "mu")
g1 <- qplot(x = mu, y = ..density.., geom = "histogram") + xlab(expression(mu))
sigma <- bi_read_var(bi_object$result$output_file_name, "sigma")
g2 <- qplot(x = sigma, y = ..density.., geom = "histogram") + xlab(expression(sigma))
grid.arrange(g1, g2)