### This demo shows how to perform filtering on a simple 
### synthetic dataset using bi_wrapper.
rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

library('ggplot2', quietly = TRUE)
library('gridExtra', quietly = TRUE)

# the PZ model file is included in RBi and can be found there:
model_file_name <- system.file(package="bi", "PZ.bi")

T <- 100
init_parameters <- list(P = 2, Z = 2, mu = 0.5, sigma = 0.3)
# First let's generate a dataset from the model
synthetic_dataset <- bi_generate_dataset(endtime=T, model_file_name=model_file_name,
                                         init=init_parameters)
# Settings
bi_object <- libbi$new(client="filter", model_file_name=model_file_name)
print(bi_object)
# Once happy with the settings, launch bi.
bi_object$run(add_options = list(nparticles = 8192, nthreads = 1, "end-time" = T, noutputs = T), verbose = T, obs = synthetic_dataset, init = init_parameters)
# It can be a good idea to look at the result file
bi_file_summary(bi_object$result$output_file_name)
bi_read(bi_object$result$output_file_name, vars = "mu")
# Let's have a look at the filtering means
# First, get the particles
logw <- xtabs(value ~ nr + np, data = bi_read(bi_object, "logweight"))
P <- bi_read(bi_object, "P")$value
Z <- bi_read(bi_object, "Z")$value
# Then compute the filtering means
w = t(apply(X=logw, MARGIN=1, FUN=log2normw))
Pmeans = apply(X = P*w, MARGIN=1, FUN=sum)
Zmeans = apply(X = Z*w, MARGIN=1, FUN=sum)
# Finally retrieve the original values used to generate the data
P_original <- bi_read(synthetic_dataset, "P")$value
Z_original <- bi_read(synthetic_dataset, "Z")$value
# And now plot the estimated states along with the original "unknown" states
# taken from the synthetic dataset
theme_set(theme_bw())
gP <- qplot(x=seq_along(Pmeans), y=Pmeans, geom = "line", col = "P") +
  geom_line(aes(y=P_original, col = "P_original")) + scale_color_discrete(name = "") +
  xlab("time") + ylab("P")
gZ <- qplot(x=seq_along(Zmeans), y=Zmeans, geom = "line", col = "Z") +
  geom_line(aes(y=Z_original, col = "Z_original")) + scale_color_discrete(name = "") +
  xlab("time") + ylab("Z")
grid.arrange(gP, gZ)

