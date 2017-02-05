context("Testing saving and retrieving libbi objects")

bi <- libbi(model = system.file(package="rbi", "PZ.bi"))
example_output_file <- system.file(package="rbi", "example_output.nc")
bi <- add_output(bi, example_output_file)

test_that("libbi object with added output is functional",
{
  expect_true(class(bi) == "libbi")
  expect_true(bi$run_flag)
  expect_true(length(bi$model[]) > 0)
})

test_that("saved and re-read libbi object functional",
{
  save_libbi(bi, "test.rds")
  bi <- read_libbi("test.rds")
  res <- bi_read(bi)
  expect_true(class(bi) == "libbi")
  expect_true(bi$run_flag)
  expect_true(length(bi$model[]) > 0)
  expect_true(is.list(res))
})

test_that("saved and re-read data frames are equal",
{
  df <- data.frame(a=c(0, 0, 1, 1), time=c(0, 1, 0, 1), value=c(7, 5, 8, 0))
  tf <- tempfile()
  bi_write(tf, list(test=df))
  df_read <- bi_read(tf)$test
  expect_true(all(df_read == df))
})

