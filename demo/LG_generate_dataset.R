### This demo shows how to generate data from a model
### using bi sample --target prior and then the gen_obs function provided in Rbi.

rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)


# General settings
settings <- bi_settings(client = "sample", 
                        args = "--target joint --model-file LG.bi --nsamples 1",
                        path_to_model = "~/workspace/lg")
T <- 1000
# First launch libbi without specifying an init-file,
# so the parameters and initial conditions are drawn from the prior distribution.
bi_result <- bi_libbi(bi_settings=settings, 
                args = paste("--end-time", T, "--noutputs", T, "--verbose --nthreads 1"),
                outputfile="results/joint.nc")
bi_file_summary(bi_result$outputfile)
Y1 <- bi_read_var(bi_result$outputfile, "Y")
X1 <- bi_read_var(bi_result$outputfile, "X")
D1 <- bi_read_var(bi_result$outputfile, "D")
g1 <- qplot(x = seq_along(Y1), y = Y1, geom = "line") +
  geom_line(aes(y=X1), col = "red") + xlab("time") + ylab(paste("Y with D=", round(D1, 2)))
# Then launch libbi with a specified init-file,
# generated using bi_init_file. The variables that are not specified in the init-file
# will be drawn from the prior, the others will be used.
initfilepath <- tempfile(pattern="initfile")
parameters <- list(D = 0.5, X = 0)
bi_init_file(initfilepath, parameters)
# Check the init file
bi_file_ncdump(initfilepath)
# Launch bi
bi_result <- bi_libbi(bi_settings=settings, 
                args = paste("--end-time", T, "--noutputs", T, "--init-file", initfilepath,
                             "--verbose --nthreads 1"),
                outputfile="results/joint.nc")
# We can check that the init-file has been taken into account
Y2 <- bi_read_var(bi_result$outputfile, "Y")
X2 <- bi_read_var(bi_result$outputfile, "X")
D2 <- bi_read_var(bi_result$outputfile, "D")
g2 <- qplot(x = seq_along(Y2), y = Y2, geom = "line") +
  geom_line(aes(y=X2), col = "red") + xlab("time") + ylab(paste("Y with D=", round(D2, 2)))
grid.arrange(g1,g2)