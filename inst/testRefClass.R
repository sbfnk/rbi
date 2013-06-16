library(bi)
bi_object <- bi_wrapper$new(client = "filter", 
                        config = "filter.conf",
                        path_to_model = "~/workspace/lg")
print(bi_object)
# Once happy with the settings, launch bi.
bi_object$libbi(add_options="--verbose --nthreads 1",
                outputfile = "results/launchLG_PF.nc", stdoutputfile = "diagnostics.txt")
bi_object$result

# It can be a good idea to look at the result file
bi_file_summary(bi_object$result$outputfile)




