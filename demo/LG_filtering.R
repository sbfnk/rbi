rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

init_file_name <- "data/init1.nc"

bi_object <- bi_wrapper$new(client = "filter", 
                            config = "filter.conf",
                            path_to_model = "~/workspace/lg",
                            global_options = paste0("--init-file=", init_file_name))
print(bi_object)
bi_init_file(filename=paste0(bi_object$path_to_model, "/", init_file_name), 
             variables=list(A = 0.9, B = 1, D = 1, X = 0))

outputPF <- "/home/pierre/workspace/lg/results/launchLG_PF.nc"
bi_object$run(add_options = "--verbose --nthreads 1",
   outputfile = outputPF, stdoutputfile = "diagnostics.txt")
# It can be a good idea to look at the result file
bi_file_summary(bi_object$result$outputfile)

# Then we can plot some variable
logw <- bi_read_var(bi_object$result$outputfile, "logweight")
X <- bi_read_var(bi_object$result$outputfile, "X")
w <- apply(X=logw, MARGIN=2, FUN=log2normw)

Xmeans = apply(X = X*w, MARGIN=2, FUN=sum)
qplot(x=seq_along(Xmeans), y=Xmeans, geom = "line", col = "X") +
  scale_color_discrete(name = "") +
  xlab("time") + ylab("Hidden state")

outputKF <- "/home/pierre/workspace/lg/results/launchLG_KF.nc"
bi_object$run(add_options = "--verbose --nthreads 1 --filter=kalman",
              outputfile = outputKF)

matplot(Xmeans, type ="l")

