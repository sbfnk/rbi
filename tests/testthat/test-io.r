context("Testing saving and retrieving libbi objects")

model_str <- "
model test {
  const no_a = 2
  const no_b = 2
  dim a(no_a)
  dim b(no_b)

  obs M[a]

  state N[a] (has_input = 0)
  noise e[a, b]
  param m[a, b]

  sub parameter {
    m[a,b] ~ truncated_gaussian(lower=0)
  }

  sub initial {
    N[a] <- 1
  }

  sub transition {
    e[a, b] ~ gaussian(mean = m[a,b])
    N[a] <- N[a] + e[a, 0] +
       e[a, 1]
  }

  sub observation {
    inline x = m
    M[a] ~ gaussian(mean = N[a])
  }
}
"

model <- bi_model(lines = stringi::stri_split_lines(model_str)[[1]])
bi <- libbi(model, dims=list(a=c("first", "second")))
test_output <-
  Map(
    function(x) { if (is.data.frame(x)) x$value <- rnorm(nrow(x)); x },
    list(e=data.frame(expand.grid(time=0:1, b=0:1, a=c("first", "second"), np=0:1)),
         N=data.frame(expand.grid(time=0:1, a=c("first", "second"), np=0:1)),
         m=9,
         M=data.frame(expand.grid(time=0:1, a=c("first", "second"), np=0:1)), 
         loglikelihood=data.frame(np=0:1),
         logprior=data.frame(np=0:1)))
bi <- attach_data(bi, "output", test_output)
bi <- attach_data(bi, "obs", test_output["M"])
bi <- attach_data(bi, file="init", test_output[c("e", "m")])

df <- data.frame(a=c(0, 0, 1, 1), time=c(0, 1, 0, 1), value=c(7, 5, 8, 0))

tmpfile <- tempfile(fileext=".nc")

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
  save_libbi(bi, filename, supplement="test")
  bi <- read_libbi(filename)
  res <- bi_read(bi, clear_cache=TRUE, thin=10, missval_threshold=1e20)

  expect_true(class(bi) == "libbi")
  expect_true(bi$run_flag)
  expect_true(length(bi$model) > 0)
  expect_true(is.list(res))
  expect_output(print_log(bi), "test")
})

test_that("saved and re-read libbi object (from folder) functional",
{
  folder <- tempdir()
  name <- file.path(folder, "test.rds")
  save_libbi(bi, split = TRUE, name)
  bi <- read_libbi(name)
  res <- bi_read(bi, clear_cache=TRUE, thin=10, missval_threshold=1e20)
  expect_true(class(bi) == "libbi")
  expect_true(bi$run_flag)
  expect_true(length(bi$model[]) > 0)
  expect_true(is.list(res))
})

test_that("saved and re-read data frames are equal",
{
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
  expect_gt(length(bi_contents(bi)), 0)
  expect_gt(bi_dim_len(bi$output_file_name, "np"), 0)
  expect_equal(bi_dim_len(bi$output_file_name, "dummy"), 0)
  expect_gt(length(capture.output(bi_file_summary(bi))), 0)
  expect_gt(nrow(bi_read(bi, "N")$N), 0)
  expect_gt(nrow(bi_read(bi, file="obs")$M), 0)
  expect_gt(nrow(bi_read(bi, "N", verbose=TRUE, clear_cache=TRUE)$N), 0)
  expect_gt(suppressMessages(nrow(bi_read(bi, "N", verbose=TRUE)$N)), 0)
  expect_gt(nrow(bi_read(bi, file="obs", coord_dims=list(M="a"))$M), 0)
  expect_message(bi_write(tmpfile, list(test=df), verbose=TRUE), "Writing")
  expect_message(bi_read(bi, "N", verbose=TRUE, clear_cache=TRUE, thin = 2), "Reading")
  expect_true(is.na(bi_read(bi, file="init", missval_threshold=1)$m))
  expect_true(length(bi_read(bi, vars="time_M"))==1)
})

test_that("appending to existing files works",
{
  test_data <- list(test1=df, test2=df)
  bi_write(tmpfile, test_data[1])
  bi_write(tmpfile, test_data[2], append=TRUE)
  expect_equal(bi_read(tmpfile), test_data)
})

test_that("duplicate columns are read/written correctly",
{
  df_dup <- data.frame(a.1=c(0, 0, 1, 1), a.2=c(0, 1, 0, 1), value=c(7, 5, 8, 0))
  colnames(df_dup)[1:2] <- c("a", "a")
  df_dup_error <- data.frame(a.1=c(0, 0, 1, 1), a.2=c(0, 1, 2, 3), value=c(7, 5, 8, 0))
  bi_write(tmpfile, list(test=df_dup))
  expect_equal(unname(df_dup), unname(bi_read(tmpfile)$test))
  expect_error(bi_write(tmpfile, list(test=df_dup_error)), "different lengths")
  expect_error(bi_write(tmpfile, list(test=df_dup, vec=c(2, 3))), "length 1")
})

test_that("data with ns column are read/written correctly",
{
  df_ns <- data.frame(ns=c(0, 0, 1, 1), time=c(0, 1, 0, 1), value=c(7, 5, 8, 0))
  bi_write(tmpfile, list(test=df_ns))
  expect_equal(df_ns, bi_read(tmpfile)$test)
})

test_that("guessing time and coordinate dimensions works",
{
  dims <- bi_write(tmpfile, list(test=df), guess_time=TRUE, guess_coord=TRUE)
  df_time <- data.frame(time=c(0, 1, 2, 3), value=c(7, 5, 8, 0))
  df_twotime <- data.frame(a=c(0, 0, 1, 1), time1=c(0, 1, 0, 1), time2=c(0, 0, 0, 0), value=c(7, 5, 8, 0))
  df_coord <- data.frame(a=c(0, 0, 1, 1), coord=c(0, 1, 0, 1), value=c(7, 5, 8, 0))
  df_twocoord <- data.frame(a=c(0, 0, 1, 1), b=c(2, 3, 2, 3), ns=c(0, 0, 0, 0), value=c(7, 5, 8, 0))
  expect_equal(dims$time_dim, "time")
  expect_equal(dims$coord_dims$test, "a")
  expect_equal(bi_write(tmpfile, list(test=df_time), guess_time=TRUE)$time_dim, "time")
  expect_equal(bi_write(tmpfile, list(test=df_twocoord), guess_coord=TRUE)$coord_dims$test, c("a", "b"))
  expect_error(bi_write(tmpfile, list(test=df), time_dim="a"), "time dimension")
  expect_error(bi_write(tmpfile, list(test=df_coord), coord_dim=list(test="a")), "coord dimension")
  expect_error(bi_write(tmpfile, list(test=df_twotime), guess_time=TRUE), "Could not decide")
})

test_that("data can be attached to libbi objects",
{
  expect_equal(class(attach_data(bi, "output", bi$output_file_name)), "libbi")
  expect_equal(class(attach_data(bi, "output", bi$output_file_name, append=TRUE)), "libbi")
  expect_equal(class(attach_data(bi, "output", bi)), "libbi")
  expect_equal(class(attach_data(bi, "output", bi, overwrite=TRUE, value_column="value")), "libbi")
  expect_equal(class(attach_data(bi, "obs", bi$output_file_name)), "libbi")
  expect_equal(class(attach_data(bi, "obs", bi)), "libbi")
  bi_no_time_dim <- bi
  bi_no_time_dim$time_dim <- character(0)
  expect_equal(class(attach_data(bi, "obs", bi_no_time_dim)), "libbi")
  expect_equal(class(attach_data(bi, "obs", bi, coord_dims = list(a="a"))), "libbi")
  expect_equal(class(attach_data(bi, "output", NULL)), "libbi")
  expect_equal(class(attach_data(bi, "input", NULL)), "libbi")
})

test_that("I/O errors/warnings are recognised",
{
  bi_in_place <- attach_data(bi, "output", test_output)
  expect_warning(attach_data(bi_in_place, "output", test_output, in_place=TRUE), "used carefully")
  bi_wrong_coord <- bi
  bi_wrong_coord$coord_dims<- list(M="b")
  expect_warning(attach_data(bi, "obs", bi_wrong_coord), "will override")
  bi_wrong_time <- bi
  bi_wrong_time$time_dim <- "dummy"
  expect_warning(attach_data(bi, "output", bi_wrong_time), "will override")
  expect_warning(bi_read(bi$output_file_name, file="output"), "file")
  expect_error(bi_read(bi, file="dummy"), "dummy")
  expect_error(bi_read(3), "must be")
  expect_error(bi_read(bi, model=bi$model), "should not be given")
  expect_error(bi_read(bi, type="obs", vars="M"), "Only one")
  expect_warning(bi_read(bi, dims=list(test=c("a", "b"))), "will override")
  expect_warning(bi_read(bi, coord_dims=list(M="b")), "will override")
  expect_error(bi_write(tmpfile, list()), "non-empty")
  expect_error(bi_write(tmpfile, list(df)), "named")
  expect_error(bi_write(tmpfile, list(test=df[, c("a", "time")])), "value")
  expect_error(bi_write(tmpfile, list(test=df), coord_dims=list(test="a"), guess_coord=TRUE), "not be given")
  expect_error(bi_write(tmpfile, list(test=df), time_dim="time", guess_time=TRUE), "not be given")
  expect_error(bi_write(tmpfile, list(test="dummy")), "data frame")
  expect_error(save_libbi(bi), "a name")
  expect_error(save_libbi(bi, split=TRUE, "___imaginary/file"), "does not exist")
  expect_error(read_libbi(), "specify a file")
})

test_that("deprecated options are recognised", 
{
  expect_error(bi_write(tmpfile, list(test=df), timed=TRUE), "deprecated")
  expect_warning(bi_read(bi, vector=TRUE), "deprecated")
  expect_warning(bi_read(bi, missval.threshold = 1e+20), "deprecated")
  expect_warning(bi_read(bi, init.to.param = TRUE), "deprecated")
})

test_that("an error is thrown if duplicate options are given", 
{
    expect_error(suppressWarnings(bi_read(bi, missval.threshold = 1e+20, missval_threshold = 1e+20, "Can't give")))
    expect_error(suppressWarnings(bi_read(bi, init.to.param = TRUE, init_to_param = TRUE, "Can't give")))
})

