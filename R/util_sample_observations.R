#' @rdname sample_observations
#' @name sample_observations
#' @title Sample observations from trajectories of a libbi model, one at each time step
#' @param wrapper a \code{\link{libbi}} that has been run
#' @param output_file_name the name of the output file to write
#' @export
#' 
sample_observations <- function(wrapper, output_file_name){
  if (!wrapper$run_flag) {
    stop("The model should be run first")
  }

  times <- bi_dim_vals(wrapper$output_file_name, "nr")
  iterations <- bi_dim_vals(wrapper$output_file_name, "np")
  end_time <- max(times)
  start_time <- min(times)

  wrapper$run(add_options = list("input-file" = wrapper$output_file_name,
                                 "input-np" = -1,
                                 "init-file" = wrapper$output_file_name,
                                 "init-np",
                                 "start-time" = start_time, 
                                 "end-time" = end_time, 
                                 "noutputs" = start_time - end_time, 
                                 target = "joint",
                                 nsamples = iterations),
              output_file_name = output_file_name)

  res <- bi_read_file(output_file_name)
  for (var in names(res)) {
      if ("nr" %in% names(res[[var]])) {
          res[[var]]$nr <- res[[var]]$nr - 1
          res[[var]] <- res[[var]][res[[var]]$nr >= 0, ]
      }
  }
  bi_write_file(output_file_name, res)
}
