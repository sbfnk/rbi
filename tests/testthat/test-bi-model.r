context("Testing bi_model manipulation")

model_file_name <- system.file(package="rbi", "PZ.bi")
PZ <- bi_model(filename = model_file_name)

test_that("models can be created",
{
  expect_true(length(PZ[]) > 0)
})

test_that("empty models can be created",
{
  expect_true(is_empty(bi_model()))
})

test_that("outputs can be enabled",
{
    output_disabled <- PZ
    output_disabled[6] <- "param mu (has_output=0)"
    expect_false(any(grepl("has_output", enable_outputs(output_disabled))))
    expect_error(enable_outputs("test"), "bi_model")
    expect_error(enable_outputs(PZ, type=c("all", "param")), "all")
})

test_that("parameters can be fixed",
{
  expect_true(length(fix(PZ, alpha=0, dummy=1)) > 0)
})

test_that("lines can be inserted",
{
  expect_true(!is_empty(insert_lines(PZ, lines = "noise beta", after = 0)))
  expect_true(!is_empty(insert_lines(PZ, lines = "noise beta", after = 32)))
  expect_true(!is_empty(insert_lines(PZ, lines = "noise beta", after = 34)))
  expect_true(!is_empty(insert_lines(PZ, lines = "noise beta", before = 9)))
  expect_true(!is_empty(insert_lines(PZ, lines = "beta ~ normal()", at_beginning_of = "transition")))
  expect_true(!is_empty(insert_lines(PZ, lines = "beta ~ normal()", at_end_of = "transition")))
  expect_true(!is_empty(insert_lines(PZ, lines = "beta ~ normal()", before = "dummy")))
  expect_true(!is_empty(insert_lines(PZ, lines = "beta ~ normal()", after = "parameter")))
  expect_error(insert_lines(PZ, lines = "noise beta"))
  expect_error(insert_lines(PZ, lines = "noise beta", after=35))
  expect_error(insert_lines(PZ, lines = "beta ~ normal()", by = "transition"), "by")
})

test_that("lines can be removed",
{
  rem <- PZ
  rem[13] <- NULL
  expect_true(length(remove_lines(PZ, "parameter", only="sigma")[]) > 0)
  expect_true(length(remove_lines(PZ, 13, type="sample")[]) > 0)
  expect_true(rem != PZ)
  expect_true(rem == PZ[-13])
  expect_error(remove_lines(PZ), "what")
  expect_error(remove_lines(PZ, list()), "what")
})

test_that("strings can be replaced",
{
  expect_true(length(replace_all(PZ, "sigma", "lambda")[]) > 0)
})

test_that("models can be written to file",
{
  filename <- tempfile()
  write_model(PZ, filename)
  PZ <- bi_model(paste0(filename, ".bi"))
  expect_true(!is_empty(PZ))
})

test_that("model names can be set",
{
  expect_true(length(set_name(PZ, "new_PZ")[]) > 0)
  expect_true(length(set_name(bi_model(), "new_PZ")[]) > 0)
  expect_error(set_name(bi_model(lines="{}"), "test"), "first line")
})

test_that("models can be printed",
{
  expect_output(print(PZ), "model PZ")
  expect_output(print(bi_model()), "empty")
})

test_that("parts of a model can be extracted",
{
  expect_true(length(PZ[3:4]) == 2)
  PZ[3:4] <- c("const e = 0.4", "const m_l = 0.05")
  expect_true(length(PZ[]) > 0)
})

test_that("blocks operations work",
{
  param_block <- find_block(PZ, "parameter")
  expect_equal(PZ[param_block[-c(1, length(param_block))]],
               get_block(PZ, "parameter"))
  expect_equal(get_block(add_block(PZ, "observation", "dummy"),
                         "observation"),
               "dummy")
  expect_equal(length(get_block(PZ, "dummy")), 0)
  expect_equal(length(get_block(add_block(PZ, "dummy"), "dummy")), 0)
})

test_that("empty models don't have a name",
{
    expect_true(is.na(get_name(bi_model())))
})

test_that("variables can be converted to inputs",
{
    expect_true(any(grepl("input Z", to_input(PZ))))
})

test_that("simple errors are detected",
{
    unbalanced <- suppressWarnings(PZ[-12])
    expect_error(bi_model(filename=character(0)), "empty")
    expect_error(bi_model(filename="test", lines="model x {}"), filename)
    expect_warning(PZ[-12], "unbalanced")
    expect_warning(unbalanced, "unbalanced")
})
