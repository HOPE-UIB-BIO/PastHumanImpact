testthat::test_that("get_hvarpart_importance() extracts nested results", {
  data_hvarpart <-
    tibble::tibble(
      dataset_id = c("core_a", "core_b"),
      varhp = list(
        list(
          varhp_output = list(Total_explained_variation = 0.4),
          summary_table = tibble::tibble(
            predictor = c("human", "climate"),
            `I.perc(%)` = c(70, 30)
          )
        ),
        list(
          varhp_output = list(Total_explained_variation = 0.2),
          summary_table = tibble::tibble(
            predictor = c("human", "climate"),
            `I.perc(%)` = c(20, 80)
          )
        )
      )
    )

  res_importance <-
    get_hvarpart_importance(data_hvarpart = data_hvarpart)

  testthat::expect_s3_class(res_importance, "tbl_df")
  testthat::expect_identical(nrow(res_importance), 4L)
  testthat::expect_named(
    res_importance,
    c(
      "dataset_id",
      "predictor",
      "importance_percent",
      "total_explained_variation",
      "n_hvarpart_results"
    )
  )
  testthat::expect_equal(
    sort(res_importance[["importance_percent"]]),
    sort(c(70, 30, 20, 80))
  )
})

testthat::test_that("get_hvarpart_importance() validates its contract", {
  testthat::expect_error(
    get_hvarpart_importance(
      data_hvarpart = tibble::tibble(dataset_id = "core_a")
    ),
    "nested results"
  )
})

testthat::test_that("get_hvarpart_importance() skips failed results", {
  data_hvarpart <-
    tibble::tibble(
      dataset_id = c("failed", "valid"),
      varhp = list(
        NA,
        list(
          varhp_output = list(Total_explained_variation = 0.4),
          summary_table = tibble::tibble(
            predictor = c("human", "climate"),
            `I.perc(%)` = c(70, 30)
          )
        )
      )
    )

  res_importance <-
    get_hvarpart_importance(data_hvarpart = data_hvarpart)

  testthat::expect_identical(
    unique(res_importance[["dataset_id"]]),
    "valid"
  )
})

testthat::test_that("get_hvarpart_importance() consolidates duplicates", {
  nested_result <-
    list(
      varhp_output = list(Total_explained_variation = 0.4),
      summary_table = tibble::tibble(
        predictor = c("human", "climate"),
        `I.perc(%)` = c(70, 30)
      )
    )
  data_hvarpart <-
    tibble::tibble(
      dataset_id = c("core_a", "core_a"),
      varhp = list(nested_result, nested_result)
    )

  res_importance <-
    get_hvarpart_importance(data_hvarpart = data_hvarpart)

  testthat::expect_identical(
    res_importance[["n_hvarpart_results"]],
    c(2L, 2L)
  )
})
