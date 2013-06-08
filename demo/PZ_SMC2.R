rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)
  
# Settings
settings <- bi_settings(client = "sample", 
                        config = "posterior.conf",
                        args = "--sampler smc2",
                        path_to_model = "~/workspace/pz")
print(settings)

# Once happy with the bi_settings, launch bi.
bi_result <- bi_libbi(bi_settings=settings, args=" --end-time 25 --noutputs 25 -nsamples 256 --nparticles 128 --verbose --nthreads 1",
  outputfile = "results/launchPZ_SMC2.nc")
# It can be a good idea to look at the result file
bi_file_summary(bi_result$outputfile)
# Have a look at the posterior distribution
logweight <- bi_read_var(bi_result$outputfile, "logweight")
weight <- log2normw(logweight)
mu <- bi_read_var(bi_result$outputfile, "mu")
g1 <- qplot(x = mu, y = ..density.., weight = weight, geom = "histogram") + xlab(expression(mu))
sigma <- bi_read_var(bi_result$outputfile, "sigma")
g2 <- qplot(x = sigma, y = ..density.., weight = weight, geom = "histogram") + xlab(expression(sigma))
grid.arrange(g1, g2)