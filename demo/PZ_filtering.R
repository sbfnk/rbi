rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

# Settings
bi_object <- bi_wrapper$new(client = "filter", 
                        config = "filter.conf",
                        path_to_model = "~/workspace/pz")

print(bi_object)
# Once happy with the settings, launch bi.
bi_object$run(add_options ="--end-time 150 --nparticles 256 --verbose --nthreads 1",
                outputfile = "results/launchPZ_PF.nc")
# It can be a good idea to look at the result file
bi_file_summary(bi_object$result$outputfile)
# Have a look at the filtering distributions
logw <- bi_read_var(bi_object$result$outputfile, "logweight")
P <- bi_read_var(bi_object$result$outputfile, "P")
Z <- bi_read_var(bi_object$result$outputfile, "Z")

w = apply(X=logw, MARGIN=2, FUN=log2normw)
Pmeans = apply(X = P*w, MARGIN=2, FUN=sum)
Zmeans = apply(X = Z*w, MARGIN=2, FUN=sum)
qplot(x=seq_along(Pmeans), y=Pmeans, geom = "line", col = "P") +
geom_line(aes(y=Zmeans, col = "Z")) + scale_color_discrete(name = "") +
  xlab("time") + ylab("Hidden state")

