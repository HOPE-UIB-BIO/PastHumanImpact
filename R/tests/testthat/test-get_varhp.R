testthat::test_that("get_varhp() returns summary table from injected hvar function", {
  data_source <-
    data.frame(
      roc = c(0.1, 0.2, 0.3),
      spd = c(10, 20, 30),
      age = c(100, 200, 300),
      stringsAsFactors = FALSE
    )

  hvar_fun <-
    function(dv,
             iv,
             method,
             scale,
             type,
             var.part,
             ...) {
      list(
        Hier.part = matrix(
          c(0.1, 0.0, 0.1, 100),
          nrow = 1,
          dimnames = list(
            c("human"),
            c("Unique", "Average.share", "Individual", "I.perc(%)")
          )
        )
      )
    }

  res <-
    get_varhp(
      data_source = data_source,
      response_vars = c("roc"),
      predictor_vars = list(human = c("spd")),
      run_all_predictors = FALSE,
      get_significance = FALSE,
      hvar_fun = hvar_fun,
      perm_fun = function(...) data.frame()
    )

  testthat::expect_type(res, "list")
  testthat::expect_true(all(c("varhp_output", "summary_table") %in% names(res)))
  testthat::expect_true("predictor" %in% names(res$summary_table))
  testthat::expect_identical(dplyr::pull(res$summary_table, predictor), "human")
})

testthat::test_that("get_varhp() merges significance output when requested", {
  data_source <-
    data.frame(
      roc = c(0.1, 0.2, 0.3),
      spd = c(10, 20, 30),
      stringsAsFactors = FALSE
    )

  hvar_fun <-
    function(dv,
             iv,
             method,
             scale,
             type,
             var.part,
             ...) {
      list(
        Hier.part = matrix(
          c(0.1, 0.0, 0.1, 100),
          nrow = 1,
          dimnames = list(
            c("human"),
            c("Unique", "Average.share", "Individual", "I.perc(%)")
          )
        )
      )
    }

  perm_fun <-
    function(dv,
             iv,
             method,
             scale,
             type,
             permutations,
             series,
             verbose,
             ...) {
      data.frame(
        Individual = 0.1,
        `Pr(>I)` = "0.01 **",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

  res <-
    get_varhp(
      data_source = data_source,
      response_vars = c("roc"),
      predictor_vars = list(human = c("spd")),
      run_all_predictors = FALSE,
      get_significance = TRUE,
      hvar_fun = hvar_fun,
      perm_fun = perm_fun,
      permutations = 9
    )

  testthat::expect_true("Pr(>I)" %in% names(res$summary_table))
})

testthat::test_that("get_varhp() validates required arguments", {
  testthat::expect_error(
    get_varhp(data_source = "not_a_data_frame"),
    regexp = "must be a data frame"
  )

  data_source <-
    data.frame(
      roc = c(1, 2, 3),
      spd = c(10, 20, 30),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_varhp(
      data_source = data_source,
      response_vars = c("missing_response"),
      predictor_vars = list(human = c("spd")),
      hvar_fun = function(...) list(Hier.part = matrix(numeric(), nrow = 0)),
      perm_fun = function(...) data.frame()
    ),
    regexp = "must exist"
  )

  testthat::expect_error(
    get_varhp(
      data_source = data_source,
      response_vars = c("roc"),
      predictor_vars = list(human = c("spd")),
      hvar_fun = "not_a_function",
      perm_fun = function(...) data.frame()
    ),
    regexp = "must be a function"
  )
})
