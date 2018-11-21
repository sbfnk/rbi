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

PZ <- libbi(model = system.file(package="rbi", "PZ.bi"))
example_output_file <- system.file(package="rbi", "example_output.nc")
PZ <- attach_data(PZ, "output", example_output_file)

test_that("we can print an empty libbi object",
{
  expect_output(print(bi), "LibBi has not been run")
})

test_that("we can run libbi and analyse results",
{
  skip_on_cran()
  bi <- sample(bi, proposal="prior", start_time=0, nsamples=10, verbose=TRUE, thin=2,
               output_every=2, end_time=10)
  dry <- sample(model, dry=c("run", "gen", "parse", "build"))
  dataset <- bi_generate_dataset(bi, end_time=50, noutputs=50)
  dataset <- bi_generate_dataset(bi, end_time=50)
  dataset_r <- bi_read(dataset)
  bi1 <- sample(bi, obs=dataset_r, debug=TRUE, fix=c(e=0.5), nsamples=10, with="output-at-obs", without="gdb")
  bi2 <- sample(bi, seed=1234, model_file=bi$model_file, obs=dataset, working_folder=bi$working_folder, with="transform-initial-to-param")

  joined <- join(a=bi1, b=bi2)
  pred <- predict(bi, end_time=100)
  so <- sample_obs(bi)

  res <- bi_read(bi)
  pred_res <- bi_read(pred, thin=2)
  flat <- bi_read(so, flatten=TRUE)
  traces <- get_traces(bi, burnin=2)

  ll <- logLik(bi)

  expect_true(nrow(bi_read(dataset)[["N"]]) > 0)
  expect_true(nrow(bi_read(dataset)[["N"]]) > 0)
  expect_equal(class(bi), "libbi")
  expect_equal(class(pred), "libbi")
  expect_true(bi$run_flag)
  expect_true(length(bi$model[]) > 0)
  expect_true(is.list(res))
  expect_true(is.list(pred_res))
  expect_output(print(bi, verbose=TRUE), "path to working")
  expect_output(print_log(bi), "libbi")
  expect_equal(nrow(summary(bi)), 1)
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
  expect_warning(libbi(bi$model, sample_obs=TRUE), "sample_obs")
  expect_warning(libbi(bi$model, output_all=TRUE), "output_all")
  expect_warning(libbi(bi$model, options=list("--enable-cuda", "--nsamples=100")), "options")
})

test_that("warnings are given",
{
  skip_on_cran()

  expect_warning(libbi(model=bi$model, model_file=bi$model_file_name), "model-file")
  expect_warning(libbi(model=bi$model, model_file=bi$model_file_name), "model-file")
  expect_warning(sample(PZ, init=list(a=3), chain=TRUE, dry=c("run", "gen", "parse", "build")), "chain")
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
})

test_that("LibBi errors are caught",
{
  skip_on_cran()
  erroneous_model <- bi$model
  erroneous_model[2] <- "doodle"
  expect_error(sample(bi, model=erroneous_model))
  expect_error(rewrite(model, model=model))
})

