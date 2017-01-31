context("Testing bi_model manipulation")

model_file_name <- system.file(package="rbi", "PZ.bi")
PZ <- bi_model(filename = model_file_name)

test_that("Models can be created",
{
  expect_true(length(PZ[]) > 0)
})

test_that("Parameters can be fixed",
{
  expect_true(length(fix(PZ, alpha=0)) > 0)
})

test_that("Lines can be inserted",
{
  expect_true(length(insert_lines(PZ, lines = "noise beta", after = 8)[]) > 0)
})

test_that("Lines can be removed",
{
  expect_true(length(remove_lines(PZ, 2)[]) > 0)
})

test_that("Models can be written to file", 
{
  write_file(PZ, "PZ.bi")
  PZ <- bi_model("PZ.bi")
  expect_true(length(PZ[]) > 0)
})

test_that("Model names can be set",
{
  expect_true(length(set_name(PZ, "new_PZ")[]) > 0)
})

test_that("Parts of a model can be extracted",
{
  expect_true(length(PZ[3:4]) == 2)
  PZ[3:4] <- c("const e = 0.4", "const m_l = 0.05")
  expect_true(length(PZ[]) > 0)
})

