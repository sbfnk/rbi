# the PZ model file is included in rbi and can be found there:
model_file_name <- system.file(package = "rbi", "PZ.bi")

# assign model variable
pz <- bi_model(model_file_name)
# look at the model
pz

tf <- 50

init_parameters <- list(P = 2, Z = 2, mu = 0.5, sigma = 0.3)
# First let's generate a dataset from the model
synthetic_dataset <- bi_generate_dataset(
  pz, end_time = tf, noutputs = tf, init = init_parameters
)

# Settings
bi_object <- libbi(model = pz, sampler = "smc2")
# have a look at the object
bi_object

# Once happy with the settings, launch bi.
bi_object <- sample(
  bi_object, end_time = tf, noutputs = tf, nsamples = 128, nparticles = 128,
  nthreads = 1, obs = synthetic_dataset, init = init_parameters,
  log_file_name = tempfile(pattern = "smc2output", fileext = ".txt")
)
# It can be a good idea to look at the result file
bi_file_summary(bi_object$output_file_name)
# look at the object again
bi_object
# print summary
summary(bi_object)
# Have a look at the posterior distribution
output <- bi_read(bi_object, vars = c("logweight", "mu", "sigma"))
logweight <- output$logweight$value

log2normw <- function(lw) {
  w <- exp(lw - max(lw))
  return(w / sum(w))
}

weight <- log2normw(logweight)
mu <- output$mu$value
sigma <- output$sigma$value

par(mfrow = c(2, 1))

hist(mu, xlab = expression(mu), main = "", breaks = 30, freq = FALSE)
hist(sigma, xlab = expression(sigma), main = "", breaks = 30, freq = FALSE)
