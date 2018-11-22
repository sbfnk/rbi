context("Testing saving and retrieving libbi objects")

bi <- libbi(model = system.file(package="rbi", "PZ.bi"))
example_output <- bi_read(system.file(package="rbi", "example_output.nc"))
bi <- attach_data(bi, "output", example_output)
bi <- attach_data(bi, file="init", bi_read(bi, vars=c("mu", "sigma")))

nc <- nc_open(bi$output_file_name)

test_that("libbi object with added output is functional",
{
  expect_true(class(bi) == "libbi")
  expect_true(bi$run_flag)
  expect_true(length(bi$model[]) > 0)
})

test_that("saved and re-read libbi object functional",
{
  writeLines("test", bi$log_file_name)
  filename <- tempfile(fileext=".rds")
  save_libbi(bi, filename)
  bi <- read_libbi(filename)
  res <- bi_read(bi, clear_cache=TRUE, thin=10, missval.threshold=1e20)

  expect_true(class(bi) == "libbi")
  expect_true(bi$run_flag)
  expect_true(length(bi$model[]) > 0)
  expect_true(is.list(res))
  expect_output(print_log(bi), "test")
})

test_that("saved and re-read libbi object (from folder) functional",
{
  folder <- tempdir()
  name <- file.path(folder, "test")
  save_libbi(bi, split = TRUE, name)
  bi <- read_libbi(name)
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
  dims <- bi_write(tf, list(test=df))
  df_read <- bi_read(paste0(tf, ".nc"))$test
  expect_equal(df_read, df)

  test_value <- c(1.23)
  dims <- bi_write(tf, list(test=test_value))
  nc <- ncdf4::nc_open(paste0(tf, ".nc"))
  vec_read <- bi_read(nc)$test
  ncdf4::nc_close(nc)
  expect_lt(abs(vec_read - test_value), 1e-7)
})

test_that("basic I/O functions work", 
{
  expect_true(length(bi_contents(example_output_file)) > 0)
  expect_true(bi_dim_len(example_output_file, "np") > 0)
  expect_equal(bi_dim_len(example_output_file, "dummy"), 0)
  expect_true(length(capture.output(bi_file_summary(example_output_file))) > 0)
  expect_true(nrow(bi_read(bi, "Z")$Z) > 0)
})

test_that("I/O errors/warnings are recognised", 
{
  expect_warning(bi_read(bi$output_file_name, file="output"), "file")
  expect_error(bi_read(bi, file="dummy"), "dummy")
  expect_error(bi_read(3), "must be")
})
