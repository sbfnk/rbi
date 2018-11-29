context("Testing running libbi")

model_str <- "
model test {
  const no_a = 2
  const no_b = 2
  dim a(no_a)
  dim b(no_b)

  obs M[a]

  state N[a] (has_input = 0)
  noise e[a, b]
  param m[a, b]

  sub parameter {
    m[a,b] ~ truncated_gaussian(lower=0)
  }

  sub initial {
    N[a] <- 1
  }

  sub transition {
    e[a, b] ~ gaussian(mean = m[a,b])
    N[a] <- N[a] + e[a, 0] +
       e[a, 1]
  }

  sub observation {
    inline x = m
    M[a] ~ gaussian(mean = N[a])
  }
}
"

model <- bi_model(lines = stringi::stri_split_lines(model_str)[[1]])
bi <- libbi(model, dims=list(a=c("first", "second")))
test_output <-
  Map(
    function(x) { if (is.data.frame(x)) x$value <- abs(rnorm(nrow(x))); x },
    list(e=data.frame(expand.grid(time=0:1, a=c("first", "second"), b=0:1, np=0:1)),
         N=data.frame(expand.grid(time=0:1, a=c("first", "second"), np=0:1)),
         m=data.frame(expand.grid(a=c("first", "second"), b=0:1, np=0:1)),
         close=0,
         loglikelihood=data.frame(np=0:1),
         logprior=data.frame(np=0:1)))
bi <- attach_data(bi, "output", test_output)

config_tmp_file <- tempfile(fileext = ".conf")
writeLines("--enable-assert", config_tmp_file)
log_tmp_file <- tempfile(fileext = ".log")
output_tmp_file <- tempfile(fileext = ".nc")
model_tmp_file <- tempfile(fileext = ".bi")

test_that("we can print an empty libbi object",
{
  empty_bi <- libbi(model)
  expect_output(print(empty_bi), "LibBi has not been run")
  empty_bi$run_flag <- TRUE
  expect_output(print(empty_bi), "No output file")
})

test_that("we can find the installed libbi version",
{
  skip_on_cran()
  expect_gt(nchar(installed_libbi_version()), 0)
})

test_that("we can run libbi and analyse results",
{
  skip_on_cran()
  bi_run <- sample(bi, proposal="prior", start_time=0, nsamples=10, thin=2, output_every=2, end_time=10)
  dry <- sample(model, dry=c("run", "gen", "parse", "build"))
  dataset <- bi_generate_dataset(bi_run, end_time=50)
  dataset_r <- bi_read(dataset)
  invisible(
    capture.output(
    bi1 <-
      sample(bi_run, obs=dataset_r["M"], debug=TRUE, fix=c(e=0.5), nsamples=10,
             with="output-at-obs", without="gdb", config=config_tmp_file,
             log_file_name=log_tmp_file, output_file=output_tmp_file)
    )
  )
  write_model(bi_run$model, model_tmp_file)
  invisible(
    capture.output(
      bi2 <-
        sample(bi_run, seed=1234, obs=dataset,
               working_folder=bi_run$working_folder, with="transform-initial-to-param",
               noutputs=10, assert=FALSE, log_file_name="")
    )
  )

  joined <- join(bi2, bi1)
  pred <- predict(bi_run, end_time=100)
  so <- sample_obs(bi_run)

  res <- bi_read(bi_run)
  pred_res <- bi_read(pred, thin=2)
  flat <- bi_read(so, flatten=TRUE)
  traces <- get_traces(bi_run, burnin=2)

  ll <- logLik(bi_run)

  expect_true(nrow(bi_read(dataset)[["N"]]) > 0)
  expect_equal(class(bi_run), "libbi")
  expect_equal(class(pred), "libbi")
  expect_true(bi_run$run_flag)
  expect_true(length(bi_run$model[]) > 0)
  expect_true(is.list(res))
  expect_true(is.list(pred_res))
  expect_output(print(so, verbose=TRUE), "path to working")
  expect_output(print_log(bi_run), "libbi")
  expect_output(print_log(bi_run$log_file_name), "libbi")
  expect_equal(nrow(summary(bi_run)), 4)
  expect_equal(ncol(res$N), 4)
  expect_true(nrow(flat) > 0)
  expect_true(nrow(traces) > 0)
  expect_true(is.numeric(ll))
})

test_that("we can rewrite a model",
{
  skip_on_cran()
  expect_output(rewrite(model), "Action")
})

test_that("deprecated options are reported",
{
  expect_warning(libbi(bi$model, sample_obs=TRUE), "deprecated")
  expect_warning(libbi(bi$model, output_all=TRUE), "deprecated")
  expect_warning(libbi(bi$model, options=list("--enable-cuda", "--nsamples=100", "--with-transform-obs-to-state")), "deprecated")
  expect_warning(attach_file(bi, "input", test_output), "deprecated")
})

test_that("warnings are given",
{
  skip_on_cran()

  expect_warning(libbi(model=bi$model, model_file=bi$model_file_name), "model-file")
  expect_warning(libbi(model_file=bi$model_file_name), "model-file")
  expect_warning(run(bi, init_np=0, chain=TRUE, client=character(0)), "init-np")
  expect_warning(sample(bi, init=list(a=3), chain=TRUE, dry=c("run", "gen", "parse", "build")), "chain")
})

test_that("errors are recognised",
{
  skip_on_cran()
  expect_error(sample(bi, config="@dummy.conf"))
  expect_error(sample(bi, with="x", without="x"))
  expect_error(rewrite(model, model=model))
  expect_error(sample(model, model=model))
  expect_error(libbi(bi_model()))
  expect_error(sample(bi, noutputs=100, output_every=1))
  expect_error(print_log(libbi(model)), "does not exist")
  expect_error(print_log("___imaginary_file"), "does not exist")
  expect_error(print_log(3), "must be a 'libbi'")
  expect_error(sample_obs(3), "must be a 'libbi'")
})

test_that("LibBi objects are asserted correctly",
{
  bi_no_run_flag <- bi
  bi_no_run_flag$run_flag <- FALSE
  expect_error(assert_files(bi_no_run_flag), "must be run first")
  bi_output_modified <- bi
  bi_output_modified$timestamp$output <- bi_output_modified$timestamp$output - 1
  expect_error(assert_files(bi_output_modified), "has been modified")
  bi_init_modified <- attach_data(bi, "init", test_output)
  bi_init_modified$timestamp$init <- bi_init_modified$timestamp$init - 1
  expect_error(assert_files(bi_init_modified), "has been modified")
  bi_nonexistent <- bi
  bi_nonexistent$options$`dummy-file` <- "__imaginary"
  expect_error(assert_files(bi_nonexistent), "does not exist")
  expect_error(sample(model, model=model))
  expect_error(libbi(bi_model()))
  expect_error(sample(bi, noutputs=100, output_every=1))
  expect_error(print_log(libbi(model)), "does not exist")
  expect_error(print_log("___imaginary_file"), "does not exist")
  expect_error(print_log(3), "must be a 'libbi'")
})

test_that("LibBi errors are caught",
{
  skip_on_cran()
  erroneous_model <- bi$model
  erroneous_model[2] <- "doodle"
  expect_error(sample(bi, model=erroneous_model))
  expect_error(rewrite(model, model=model))
  expect_error(rewrite(libbi(bi_model())), "No model")
})

test_that("problems with options are identified",
{
  expect_error(option_list(3), "lists or character")
})

