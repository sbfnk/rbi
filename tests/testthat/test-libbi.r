context("Testing running libbi")

model <- system.file(package="rbi", "PZ.bi")
bi <- libbi(model)

test_that("we can print an empty libbi object",
{
  expect_output(print(bi), "LibBi has not been run")
})

test_that("we can run libbi",
{
  skip_on_cran()
  dataset <- bi_generate_dataset(model=model, end_time=50)
  expect_true(nrow(bi_read(dataset)[["P_obs"]]) > 0)
  dataset <- bi_generate_dataset(model=model, options=list(end_time=50))
  expect_true(nrow(bi_read(dataset)[["P_obs"]]) > 0)
  bi <- sample(bi, proposal="prior", options="--start-time 0")
  bi <- sample(bi, sample_obs=TRUE, obs=dataset, output_all=TRUE, fix=c(dummy=0))
  res <- bi_read(bi)
  expect_equal(class(bi), "libbi")
  expect_true(bi$run_flag)
  expect_true(length(bi$model[]) > 0)
  expect_true(is.list(res))
  expect_output(print(bi), "Number of sample")
  expect_equal(nrow(summary(bi)), 2)
})

test_that("we can rewrite a model", 
{
  skip_on_cran()
  rewrite(bi)
})
