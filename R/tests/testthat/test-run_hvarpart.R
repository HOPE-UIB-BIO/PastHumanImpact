testthat::test_that("run_hvarpart() maps get_varhp over data_merge with response vars", {
  data_source <-
    data.frame(
      dataset_id = 1,
      data_merge = I(list(data.frame(age = c(1, 2), spd = c(10, 20)))),
      stringsAsFactors = FALSE
    )

  old_get_varhp <-
    get("get_varhp", envir = .GlobalEnv)

  on.exit(
    assign("get_varhp", old_get_varhp, envir = .GlobalEnv),
    add = TRUE
  )

  assign(
    "get_varhp",
    function(data_source,
             permutations,
             response_dist,
             response_vars,
             predictor_vars,
             run_all_predictors,
             time_series,
             get_significance) {
      list(
        summary_table = data.frame(
          predictor = "human",
          Individual = 0.12,
          stringsAsFactors = FALSE
        )
      )
    },
    envir = .GlobalEnv
  )

  res_data <-
    run_hvarpart(
      data_source = data_source,
      response_vars = c("roc"),
      response_dist = NULL,
      data_response_dist = NULL,
      predictor_vars = list(human = c("spd")),
      run_all_predictors = FALSE,
      time_series = FALSE,
      get_significance = FALSE,
      permutations = 9
    )

  testthat::expect_true("varhp" %in% names(res_data))
  testthat::expect_type(dplyr::pull(res_data, varhp)[[1]], "list")
})

testthat::test_that("run_hvarpart() uses distance column when provided", {
  data_source <-
    data.frame(
      dataset_id = 1,
      data_merge = I(list(data.frame(age = c(1, 2), spd = c(10, 20)))),
      stringsAsFactors = FALSE
    )

  data_source$dist_col <-
    I(list(stats::dist(matrix(c(1, 2, 3, 4), ncol = 2))))

  old_get_varhp <-
    get("get_varhp", envir = .GlobalEnv)

  on.exit(
    assign("get_varhp", old_get_varhp, envir = .GlobalEnv),
    add = TRUE
  )

  assign(
    "get_varhp",
    function(data_source,
             data_response_dist,
             permutations,
             response_vars,
             response_dist,
             predictor_vars,
             run_all_predictors,
             time_series,
             get_significance) {
      testthat::expect_s3_class(data_response_dist, "dist")
      list(summary_table = data.frame(predictor = "human"))
    },
    envir = .GlobalEnv
  )

  res_data <-
    run_hvarpart(
      data_source = data_source,
      response_vars = NULL,
      data_response_dist = "dist_col",
      predictor_vars = list(human = c("spd")),
      run_all_predictors = FALSE,
      time_series = FALSE,
      get_significance = FALSE,
      permutations = 9
    )

  testthat::expect_true("varhp" %in% names(res_data))
})

testthat::test_that("run_hvarpart() validates required inputs", {
  testthat::expect_error(
    run_hvarpart(
      data_source = data.frame(dataset_id = 1),
      response_vars = c("roc")
    ),
    regexp = "must contain a data_merge"
  )

  data_source <-
    data.frame(
      dataset_id = 1,
      data_merge = I(list(data.frame(age = 1, spd = 2))),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    run_hvarpart(
      data_source = data_source,
      response_vars = NULL,
      data_response_dist = "missing_col"
    ),
    regexp = "must exist in"
  )
})
