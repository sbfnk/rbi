### This demo shows how to perform filtering on a simple
### synthetic dataset using libbi.
# the PZ model file is included in rbi and can be found there:
model_file_name <- system.file(package = "rbi", "PZ.bi")

# assign model variable
pz <- bi_model(model_file_name)
# look at the model
pz

tf <- 100
init_parameters <- list(P = 2, Z = 2, mu = 0.5, sigma = 0.3)
# First let's generate a dataset from the model
synthetic_dataset <- bi_generate_dataset(
  pz,
  end_time = tf, init = init_parameters
)
# Settings
bi_object <- libbi(model = pz)
bi_object
# Once happy with the settings, launch bi.
bi_object <- filter(
  bi_object,
  nparticles = 8192, nthreads = 1, end_time = tf, noutputs = tf,
  obs = synthetic_dataset, init = init_parameters
)
# It can be a good idea to look at the result file
bi_file_summary(bi_object$output_file_name)
bi_object
summary(bi_object, type = "state")
## read mu variable
bi_read(bi_object, vars = "mu")
# Let's have a look at the filtering means
# First, get the particles
output <- bi_read(bi_object)
logw <- xtabs(value ~ time + np, data = output$logweight)
p <- output$P$value
z <- output$Z$value

# Then compute the filtering means
log2normw <- function(lw) {
  w <- exp(lw - max(lw))
  return(w / sum(w))
}

w <- t(apply(X = logw, MARGIN = 1, FUN = log2normw))
p_means <- apply(X = p * w, MARGIN = 1, FUN = sum)
z_means <- apply(X = z * w, MARGIN = 1, FUN = sum)
# Finally retrieve the original values used to generate the data
synthetic_data <- bi_read(synthetic_dataset)
p_original <- synthetic_data$P$value
z_original <- synthetic_data$Z$value
# And now plot the estimated states along with the original "unknown" states
# taken from the synthetic dataset

par(mfrow = c(2, 1))
plot(
  seq_along(p_means), p_means,
  type = "l", col = "red", xlab = "time",
  ylab = "P"
)
lines(seq_along(p_means), p_original, col = "blue")
legend(
  0, max(p_means), c("P", "P_original"),
  lty = c(1, 1), col = c("red", "blue")
)
plot(
  seq_along(z_means), z_means,
  type = "l", col = "red", xlab = "time",
  ylab = "Z"
)
lines(seq_along(z_means), z_original, col = "blue")
legend(
  0, max(z_means), c("Z", "Z_original"),
  lty = c(1, 1), col = c("red", "blue")
)
