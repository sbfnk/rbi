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
    N[a] <- N[a] + e[a, 0] + e[a, 1]
  }

  sub observation {
    inline x = m
    M[a] ~ gaussian(mean = N[a])
  }
}
"

model <- bi_model(lines = stringi::stri_split_lines(model_str)[[1]])
bi <- libbi(model)

test_that("we can print an empty libbi object",
{
  expect_output(print(bi), "LibBi has not been run")
})

test_that("we can run libbi and analyse results",
{
  skip_on_cran()
  bi <- sample(bi, proposal="prior", start_time=0, nsamples=10, dry="run")
  dataset <- bi_generate_dataset(model=model, end_time=50)
  expect_true(nrow(bi_read(dataset)[["N"]]) > 0)
  dataset <- bi_generate_dataset(model=model, end_time=50,
                                 dims=list(a=c("first", "second")))
  dataset_r <- bi_read(dataset)
  expect_true(nrow(bi_read(dataset)[["N"]]) > 0)
  bi <- sample(model, obs=dataset_r, output_all=TRUE, fix=c(e=0.5), nsamples=10, with="output-at-obs", without="gdb")
  bi2 <- sample(bi, seed=1234, model_file=bi$model_file, obs=dataset, working_folder=bi$working_folder, with="transform-initial-to-param")

  bi <- join(a=bi, b=bi2)
  pred <- predict(bi, end_time=100)

  res <- bi_read(bi)
  pred_res <- bi_read(pred, thin=2)

  traces <- get_traces(bi, burnin=2)

  ll <- logLik(bi)

  expect_equal(class(bi), "libbi")
  expect_equal(class(pred), "libbi")
  expect_true(bi$run_flag)
  expect_true(length(bi$model[]) > 0)
  expect_true(is.list(res))
  expect_true(is.list(pred_res))
  expect_output(print(bi, verbose=TRUE), "path to working")
  expect_equal(nrow(summary(bi)), 1)
  expect_equal(ncol(res$N), 4)
  expect_true(nrow(traces) > 0)
  expect_true(is.numeric(ll))
})

test_that("we can rewrite a model",
{
  skip_on_cran()
  expect_match(rewrite(bi), ".")
})

test_that("errors are recognised",
{
  skip_on_cran()
  expect_error(sample(bi, config="@dummy.conf"))
  expect_error(sample(bi, with="x", without="x"))
})

