context("Testing running libbi")

test_that("Sampling from a libbi object",
{
  skip_on_cran()
  system("env")
  bi <- libbi(model = system.file(package="rbi", "PZ.bi"))
  bi <- sample(bi, verbose=TRUE)
  res <- bi_read(bi)
  expect_true(class(bi) == "libbi")
  expect_true(bi$run_flag)
  expect_true(length(bi$model[]) > 0)
  expect_true(is.list(res))
})

