context("Testing running libbi")

model_str <- "
model test {
  const no_a = 2
  dim a(no_a)

  obs M[a]

  state N[a] (has_input = 0)
  noise e
  param m

  sub parameter {
    m ~ gaussian()
  }

  sub initial {
    N[a] <- 1
  }

  sub transition {
    e ~ gaussian(mean = m)
    N[a] <- N[a] + e
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

test_that("we can run libbi",
{
  skip_on_cran()
  dataset <- bi_generate_dataset(model=model, end_time=50)
  expect_true(nrow(bi_read(dataset)[["N"]]) > 0)
  dataset <- bi_generate_dataset(model=model, options=list(end_time=50))
  expect_true(nrow(bi_read(dataset)[["N"]]) > 0)
  bi <- sample(bi, proposal="prior", options="--start-time 0")
  bi <- sample(model, sample_obs=TRUE, obs=dataset, output_all=TRUE, fix=c(e=0.5))
  res <- bi_read(bi)
  expect_equal(class(bi), "libbi")
  expect_true(bi$run_flag)
  expect_true(length(bi$model[]) > 0)
  expect_true(is.list(res))
  expect_output(print(bi), "Number of sample")
  expect_equal(nrow(summary(bi)), 1)
  expect_equal(ncol(bi_read(bi)$M), 3)
})

test_that("we can rewrite a model",
{
  skip_on_cran()
  rewrite(bi)
})

