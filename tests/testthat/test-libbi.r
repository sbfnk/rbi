context("Testing running libbi")

model_str <- "
model test {
  const no_a = 2
  dim a(no_a)

  obs M[a]

  state N[a] (has_input = 0)
  noise e[a]
  param m[a]

  sub parameter {
    m[a] ~ gaussian()
  }

  sub initial {
    N[a] <- 1
  }

  sub transition {
    e[a] ~ gaussian(mean = m[a])
    N[a] <- N[a] + e[a]
  }

  sub observation {
    inline x = m
    M[a] ~ gaussian(mean = N[a])
  }
}
"

model <- bi_model(lines = stringi::stri_split_lines(model_str)[[1]])

test_that("we can print an empty libbi object",
{
  bi <- libbi(model)
  expect_output(print(bi), "LibBi has not been run")
})

test_that("we can run libbi",
{
  skip_on_cran()
  dataset <- bi_generate_dataset(model=model, end_time=50)
  expect_true(nrow(bi_read(dataset)[["N"]]) > 0)
  dataset <- bi_generate_dataset(model=model, options=list(end_time=50),
                                 dims=list(a=c("first", "second")))
  dataset_r <- bi_read(dataset)
  expect_true(nrow(bi_read(dataset)[["N"]]) > 0)
  bi <- sample(bi, proposal="prior", options="--start-time 0", nsamples=10)
  bi <- sample(model, sample_obs=TRUE, obs=dataset_r, output_all=TRUE, fix=c(e=0.5), nsamples=10)
  res <- bi_read(bi)
  res <- bi_read(bi, thin=2)
  expect_equal(class(bi), "libbi")
  expect_true(bi$run_flag)
  expect_true(length(bi$model[]) > 0)
  expect_true(is.list(res))
  expect_output(print(bi), "Number of sample")
  expect_equal(nrow(summary(bi)), 1)
  expect_equal(ncol(bi_read(bi, thin=2)$M), 4)
})

test_that("we can rewrite a model",
{
  skip_on_cran()
  rewrite(bi)
})

