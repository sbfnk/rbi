context("Testing bi_model manipulation")

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

  sub initial {N[a] <- 1}

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

test_that("models can be created", {
  expect_true(length(model) > 0)
  expect_true(is_empty(bi_model()))
  expect_error(bi_model(filename = character(0)), "empty")
  expect_error(bi_model(filename = "test", lines = "model x {}"), "filename")
})

test_that("outputs can be enabled", {
  output_disabled <- model
  output_disabled[9] <- "param m[a, b] (has_output=0)"
  expect_false(any(grepl("has_output", enable_outputs(output_disabled))))
  expect_error(enable_outputs("test"), "bi_model")
  expect_error(enable_outputs(model, type = c("all", "param")), "all")
})

test_that("parameters can be fixed", {
  expect_equal(get_const(fix(model, m = 0, dummy = 1))[["m"]], 0)
  expect_equal(get_const(fix(model, m = "test", dummy = 1))[["m"]], "test")
})

test_that("lines can be inserted", {
  expect_true(!is_empty(insert_lines(model, lines = "noise beta", after = 0)))
  expect_true(!is_empty(insert_lines(model, lines = "noise beta", after = 22)))
  expect_true(!is_empty(insert_lines(model, lines = "noise beta", after = 24)))
  expect_true(!is_empty(insert_lines(model, lines = "noise beta", before = 9)))
  expect_true(
    !is_empty(
       insert_lines(
         model, lines = "beta ~ normal()", at_beginning_of = "transition"
       )
     )
  )
  expect_true(
    !is_empty(
       insert_lines(model, lines = "beta ~ normal()", at_end_of = "transition")
     )
  )
  expect_true(
    !is_empty(insert_lines(model, lines = "beta ~ normal()", before = "dummy"))
  )
  expect_true(
    !is_empty(
       insert_lines(model, lines = "beta ~ normal()", after = "parameter")
     )
  )
  expect_error(insert_lines(model, lines = "noise beta"))
  expect_error(insert_lines(model, lines = "noise beta", after = 35))
  expect_error(
    insert_lines(model, lines = "beta ~ normal()", by = "transition"), "by"
  )
})

test_that("lines can be removed", {
  rem <- model
  rem[11] <- NULL
  expect_equal(
    length(
      get_block(
        remove_lines(model, "transition", only = "N"), "transition"
      )
    ), 1
  )
  expect_equal(
    length(
      get_block(
        remove_lines(model, 17, type = "sample"), "transition"
      )
    ), 1
  )
  expect_equal(
    length(
      get_block(
        remove_lines(model, 17, type = "assignment"), "transition"
      )
    ), 2
  )
  expect_equal(
    length(
      get_block(
        remove_lines(model, "initial", type = "assignment"), "initial"
      )
    ), 0
  )
  expect_equal(
    length(
      get_block(
        remove_lines(
          model, "parameter", preserve_shell = TRUE
        ), "parameter", shell = TRUE
      )
    ), 2
  )
  expect_true(rem != model)
  expect_true(rem == model[-11])
  expect_error(remove_lines(model), "what")
  expect_error(remove_lines(model, list()), "what")
})

test_that("strings can be replaced", {
  expect_true(length(replace_all(model, "sigma", "lambda")[]) > 0)
})

test_that("models can be written to file", {
  filename <- tempfile()
  write_model(model, filename)
  read_model <- bi_model(paste0(filename, ".bi"))
  expect_equal(model, read_model)
})

test_that("model names can be set", {
  expect_gt(length(set_name(model, "new_test")), 0)
  expect_gt(length(set_name(bi_model(), "new_tes")), 0)
  expect_error(set_name(bi_model(lines = "{}"), "test"), "first line")
})

test_that("models can be printed", {
  expect_output(print(model), "model test")
  expect_output(print(bi_model()), "empty")
})

test_that("parts of a model can be extracted", {
  expect_equal(length(model[3:4]), 2)
  model[3:4] <- c("const no_a = 1", "const no_a = 1")
  expect_true(length(model) > 0)
})

test_that("block operations work", {
  param_block <- find_block(model, "parameter")
  expect_equal(
    model[param_block[-c(1, length(param_block))]],
    get_block(model, "parameter")
  )
  expect_equal(
    get_block(add_block(model, "observation", "dummy"), "observation"), "dummy"
  )
  expect_equal(length(get_block(model, "dummy")), 0)
  expect_equal(length(get_block(add_block(model, "dummy"), "dummy")), 0)
  expect_error(get_block(model), "name")
})

test_that("empty models don't have a name", {
  expect_true(is.na(get_name(bi_model())))
})

test_that("variables can be converted to inputs", {
  expect_true(any(grepl("input N", to_input(model))))
  model_no_dim <- remove_lines(model, grep("dim ", model))
  expect_true(any(grepl("input N", to_input(model_no_dim))))
})

test_that("dimensions can be identified", {
  numeric_model <- model
  numeric_model[4] <- "dim a(2)"
  character_model <- model
  character_model[2] <- "dim b(input_dim)"
  expect_equal(get_dims(model), list(a = 2, b = 2))
  expect_equal(get_dims(numeric_model), list(a = 2, b = 2))
  expect_error(get_dims(character_model), "determine size")
})

test_that("unbalanced braces are detected", {
  unbalanced <- suppressWarnings(model[-12])
  expect_warning(model[-12], "unbalanced")
  expect_warning(capture.output(print(unbalanced)), "unbalanced")
})
