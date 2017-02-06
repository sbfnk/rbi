### This demo shows how to perform filtering on a simple 
### synthetic dataset using libbi.
# the PZ model file is included in rbi and can be found there:
model_file_name <- system.file(package="rbi", "PZ.bi")

# assign model variable
PZ <- bi_model(model_file_name)
# look at the model
PZ

T <- 50

init_parameters <- list(P = 2, Z = 2, mu = 0.5, sigma = 0.3)
# First let's generate a dataset from the model
synthetic_dataset <- bi_generate_dataset(end_time=T, model=PZ, noutputs=T, 
                                         init=init_parameters)
# Settings
bi_object <- libbi(model=PZ)
# look at the object
bi_object

# launch libbi.
bi_object <- sample(bi_object, obs=synthetic_dataset, init=init_parameters,
                    end_time=T, noutputs=T, nsamples=128, nparticles=128,
                    nthreads=1, log_file_name=tempfile(pattern="pmmhoutput", fileext=".txt"))
# It can be a good idea to look at the result file
bi_file_summary(bi_object$output_file_name)
# look at the object again
bi_object
# print summary
summary(bi_object)
# Have a look at the posterior distribution
output <- bi_read(bi_object, c("mu", "sigma"))
mu <- output$mu$value
sigma <- output$sigma$value

par(mfrow = c(2, 1))

hist(mu, xlab = expression(mu), main = "", breaks = 30, freq = FALSE)
hist(sigma, xlab = expression(sigma), main = "", breaks = 30, freq = FALSE)

## or plot using rbi.helpers
##
## library('rbi.helpers')

## ## plot filtered trajectories
## plot(bi_object)

## ## other plots
## p <- plot(bi_object, densities = "histogram")
## p$densities

## reproduce plot from above

## p_mu <- plot(bi_object, densities = "histogram", params = "mu")
## p_sigma <- plot(bi_object, densities = "histogram", params = "sigma")

## g1 <- p_mu$densities + xlab(expression(mu)) + theme(strip.background = element_blank(), strip.text = element_blank())
## g2 <- p_sigma$densities + xlab(expression(sigma)) + theme(strip.background = element_blank(), strip.text = element_blank())

## grid.arrange(g1, g2)
