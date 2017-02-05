context("Testing running libbi")

model <- system.file(package="rbi", "PZ.bi")

test_that("we can generate a simple dataset",
{
  skip_on_cran()
  dataset <- bi_generate_dataset(model=model, end_time=50)
  expect_true(nrow(bi_read(dataset)[["P_obs"]]) > 0)
})

test_that("we can sample from a libbi object",
{
  skip_on_cran()
  bi <- libbi(model)
  bi <- sample(bi)
  res <- bi_read(bi)
  expect_true(class(bi) == "libbi")
  expect_true(bi$run_flag)
  expect_true(length(bi$model[]) > 0)
  expect_true(is.list(res))
})

