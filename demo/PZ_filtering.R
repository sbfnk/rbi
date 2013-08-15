### This demo shows how to perform filtering on a simple 
### synthetic dataset using bi_wrapper.
rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

# first retrieve a simple model such as the PZ model
# if you don't have it already:
# git copy https://github.com/lawmurray/PZ.git
path_to_model <- "~/workspace/pz/"
modelfile <- "PZ.bi"

T <- 100
init_file_name <- tempfile(pattern = "init", fileext=".nc")
init_parameters <- list(P = 2, Z = 2, mu = 0.5, sigma = 0.3)
bi_init_file(filename=init_file_name, variables=init_parameters)
# First let's generate a dataset from the model
synthetic_dataset <- bi_generate_dataset(endtime=T, modelfile=modelfile,
                                         path_to_model=path_to_model,
                                         init_file_name=init_file_name)
# Settings
bi_object <- bi_wrapper$new(client="filter", 
                            global_options=paste("--model-file", modelfile, 
                                                 "--obs-file", synthetic_dataset,
                                                 "--init-file", init_file_name),
                            path_to_model=path_to_model)
print(bi_object)
# Once happy with the settings, launch bi.
bi_object$run(add_options= paste("--nparticles 8192 --verbose --nthreads 1 --end-time", T))
# It can be a good idea to look at the result file
bi_file_summary(bi_object$result$output_file_name)
bi_read_var(bi_object$result$output_file_name, "mu")
# Let's have a look at the filtering means
# First, get the particles
logw <- bi_read_var(bi_object$result$output_file_name, "logweight")
P <- bi_read_var(bi_object$result$output_file_name, "P")
Z <- bi_read_var(bi_object$result$output_file_name, "Z")
# Then compute the filtering means
w = t(apply(X=logw, MARGIN=1, FUN=log2normw))
Pmeans = apply(X = P*w, MARGIN=1, FUN=sum)
Zmeans = apply(X = Z*w, MARGIN=1, FUN=sum)
# Finally retrieve the original values used to generate the data
P_original <- bi_read_var(synthetic_dataset, "P")
Z_original <- bi_read_var(synthetic_dataset, "Z")
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

