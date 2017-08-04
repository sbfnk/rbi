context("Testing saving and retrieving libbi objects")

bi <- libbi(model = system.file(package="rbi", "PZ.bi"))
example_output_file <- system.file(package="rbi", "example_output.nc")
bi <- attach_file(bi, "output", example_output_file)

test_that("we can't add output twice'",
{
  expect_error(attach_file(bi, "output", example_output_file))
  expect_equal(class(attach_file(bi, example_output_file, force=TRUE)), "libbi")
})

test_that("libbi object with added output is functional",
{
  expect_true(class(bi) == "libbi")
  expect_true(bi$run_flag)
  expect_true(length(bi$model[]) > 0)
})

test_that("saved and re-read libbi object functional",
{
  filename <- tempfile(fileext=".rds")
  save_libbi(bi, filename)
  bi <- read_libbi(filename)
  res <- bi_read(bi, clear_cache=TRUE, thin=10, missval.threshold=1e20)
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
  df_read <- bi_read(paste0(tf, ".nc"))$test
  expect_equal(df_read, df)
  test_values <- c(0, 4, 5, 6)
  bi_write(tf, list(test=test_values))
  df_read <- bi_read(paste0(tf, ".nc"))$test
  expect_equal(df_read$value, test_values)
})

test_that("basic I/O functions work", 
{
  expect_true(length(bi_contents(example_output_file)) > 0)
  expect_true(bi_dim_len(example_output_file, "np") > 0)
  expect_equal(bi_dim_len(example_output_file, "dummy"), 0)
  expect_true(length(capture.output(bi_file_summary(example_output_file))) > 0)
})
