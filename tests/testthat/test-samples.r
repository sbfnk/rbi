context("Testing getting samples from libbi output file")

model <- system.file(package="rbi", "PZ.bi")
example_output <- bi_read(system.file(package="rbi", "example_output.nc"))
bi <- libbi(model)
bi <- attach_data(bi, "output", example_output)

test_that("we can extract a sample from an output file",
{
  expect_true(nrow(extract_sample(bi, 13)[["Z"]]) > 0)
  expect_true(nrow(extract_sample(example_output)[["Z"]]) > 0)
})

test_that("we can get the traces of an output file",
{
  bi$options$`with-transform-initial-to-param` <- TRUE
  example_output$test <- 3
  expect_gt(nrow(get_traces(bi, burnin=10)), 0)
  expect_gt(nrow(get_traces(example_output, model=model, burnin=0.1)), 0)
  expect_gt(nrow(get_traces(example_output, all=TRUE)), 0)
})

test_that("warnings and errors are given",
{
  expect_error(extract_sample(bi, 128), "greater than the maximum")
  expect_error(extract_sample(3), "must be a 'libbi' object")
  expect_warning(get_traces(bi, bi$model), "model will overwrite model")
  expect_error(get_traces(example_output), "'all' must be set to TRUE")
  expect_error(get_traces(3, all=TRUE), "must be a 'libbi' object")
  expect_error(get_traces(bi, burnin=0), "must be greater than 0")
})

