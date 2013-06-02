### This demo shows how to generate data from a model
### using bi sample --target prior and then the gen_obs function provided in Rbi.

rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)


# Settings
settings <- bi_settings(client = "sample", 
                        args = "--target joint --model-file LG.bi --nsamples 1",
                        path_to_model = "~/workspace/lg")

T <- 1000
# Settings
bi_result <- bi(bi_settings=settings, args = paste("--end-time", T, "--noutputs", T, "--verbose --nthreads 1"), outputfile="results/joint.nc")
bi_file_summary(bi_result$outputfile)
Y <- bi_read_var(bi_result$outputfile, "Y")
X <- bi_read_var(bi_result$outputfile, "X")
D <- bi_read_var(bi_result$outputfile, "D")
qplot(x = seq_along(Y), y = Y, geom = "line") +
  geom_line(aes(y=X), col = "red") + xlab("time") + ylab(paste("Y with D=", round(D, 2)))