rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

# Settings
settings <- bi_settings(client = "filter", 
                        config = "filter.conf",
                        path_to_model = "~/workspace/lg")
print(settings)
# Once happy with the settings, launch bi.
bi_result <- bi(bi_settings=settings, args="--verbose --nthreads 1",
   outputfile = "results/launchLG_PF.nc", stdoutputfile = "diagnostics.txt")
# It can be a good idea to look at the result file
bi_file_summary(bi_result$outputfile)
# Then we can plot some variable
logw <- bi_read_var(bi_result$outputfile, "logweight")
X <- bi_read_var(bi_result$outputfile, "X")
w <- apply(X=logw, MARGIN=2, FUN=log2normw)

Xmeans = apply(X = X*w, MARGIN=2, FUN=sum)
qplot(x=seq_along(Xmeans), y=Xmeans, geom = "line", col = "X") +
  scale_color_discrete(name = "") +
  xlab("time") + ylab("Hidden state")
