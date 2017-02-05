context("Testing getting samples from libbi output file")

model <- system.file(package="rbi", "PZ.bi")
example_output_file <- system.file(package="rbi", "example_output.nc")

test_that("we can extract a sample from an output file",
{
  expect_true(nrow(extract_sample(example_output_file, 13)[["Z"]]) > 0)
})

test_that("we can get the traces of an output file",
{
  traces <- get_traces(example_output_file, model=model, burnin=10)
  expect_true(nrow(traces) > 0)
})

