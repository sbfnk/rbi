### This demo shows how to perform filtering on a simple 
### synthetic dataset using libbi.
rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:rbi, unload = TRUE), silent = TRUE)
library(rbi, quietly = TRUE)

# the PZ model file is included in rbi and can be found there:
model_file_name <- system.file(package="rbi", "PZ.bi")

# assign model variable
PZ <- bi_model(model_file_name)
# look at the model
PZ

T <- 100
init_parameters <- list(P = 2, Z = 2, mu = 0.5, sigma = 0.3)
# First let's generate a dataset from the model
synthetic_dataset <- bi_generate_dataset(end_time=T, model=PZ,
                                         init=init_parameters)
# Settings
bi_object <- libbi(model=PZ)
bi_object
# Once happy with the settings, launch bi.
bi_object <- filter(bi_object, nparticles = 8192, nthreads = 1, end_time = T, noutputs = T, obs = synthetic_dataset, init = init_parameters)
# It can be a good idea to look at the result file
bi_file_summary(bi_object)
bi_object
summary(bi_object)
## read mu variable
bi_read(bi_object, vars = "mu")
# Let's have a look at the filtering means
# First, get the particles
output <- bi_read(bi_object)
logw <- xtabs(value ~ time + np, data = output$logweight)
P <- output$P$value
Z <- output$Z$value
# Then compute the filtering means
w = t(apply(X=logw, MARGIN=1, FUN=log2normw))
Pmeans = apply(X = P*w, MARGIN=1, FUN=sum)
Zmeans = apply(X = Z*w, MARGIN=1, FUN=sum)
# Finally retrieve the original values used to generate the data
synthetic_data <- bi_read(synthetic_dataset)
P_original <- synthetic_data$P$value
Z_original <- synthetic_data$Z$value
# And now plot the estimated states along with the original "unknown" states
# taken from the synthetic dataset

par(mfrow = c(2, 1))
plot(seq_along(Pmeans), Pmeans, type = "l", col = "red", xlab = "time", ylab = "P")
lines(seq_along(Pmeans), P_original, col = "blue")
legend(0, max(Pmeans), c("P", "P_original"), lty = c(1, 1), col = c("red", "blue"))
plot(seq_along(Zmeans), Zmeans, type = "l", col = "red", xlab = "time", ylab = "Z")
lines(seq_along(Zmeans), Z_original, col = "blue")
legend(0, max(Zmeans), c("Z", "Z_original"), lty = c(1, 1), col = c("red", "blue"))


