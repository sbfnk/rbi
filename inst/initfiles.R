# this data file can now be given to LibBi with the '--obs-file' option.
rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

init_file_name <- tempfile(pattern="init", fileext=".nc")

rhos <- seq(from=0.1, to=0.9, by=0.1)

bi_init_file(filename=init_file_name, 
             variables=list(rho = rhos))
bi_file_ncdump(init_file_name)
bi_object <- bi_wrapper$new(client = "filter", 
                            config = "filter.conf",
                            path_to_model = "~/workspace/HiddenAR/",
                            global_options = paste0("--init-file=", init_file_name))
outputPF <- tempfile(pattern="pfilter", fileext=".nc")
outputKF <- tempfile(pattern="kfilter", fileext=".nc")
bi_object$run(add_options = "--verbose --nthreads 1 --init-ns=1 --nparticles 2 --end-time 2",
              output_file_name = outputPF)
T <- 100
Nparticles <- 1024
Nsim <- 25
Particle_filter_LL <- matrix(0, nrow = length(rhos), ncol = Nsim)

for (index in 1:length(rhos)){
  for (isim in 1:Nsim){
    bi_object$run(add_options = paste0("--nthreads=1 --init-ns=", index-1, " --nparticles=", Nparticles, " --end-time=", T, " --dry-parse"),
                output_file_name = outputPF)
    Particle_filter_LL[index, isim] <- bi_read_var(resultfile=outputPF, name="LL")
  }
}
Kalman_LL <- rep(0, length(rhos))
bi_object$run(add_options = paste0("--verbose --nthreads 1 --filter=kalman --init-ns=", index-1, " --end-time=", 2),
              output_file_name = outputKF)
for (index in 1:length(rhos)){
  bi_object$run(add_options = paste0("--verbose --nthreads 1 --filter=kalman --init-ns=", index-1, " --end-time=", T, " --dry-parse"),
                output_file_name = outputKF)
  Kalman_LL[index] <- bi_read_var(resultfile=outputKF, name="LL")
}
# save(Kalman_LL, Particle_filter_LL, file="~/likelihoods.RData")

# Kalman_LL
# Particle_filter_LL
library(ggplot2)
PF_dataframe <- reshape::melt(Particle_filter_LL)
head(PF_dataframe)
names(PF_dataframe) <- c("param", "run", "loglikelihood")
PF_dataframe[,"param"] <- rep(rhos, Nsim)
ggplot(PF_dataframe, aes(x = param, y = loglikelihood, colour = run)) + geom_point() +
  geom_line(aes(x = rhos, y = Kalman_LL), col = "red")
