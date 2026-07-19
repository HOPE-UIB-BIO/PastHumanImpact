testthat::test_that("prepare_pap_model_data() returns long PAP model data", {
  data_properties <-
    tibble::tibble(
      dataset_id = c("d1", "d2", "d3"),
      data_merge = list(
        tibble::tibble(
          age = c(0, 500),
          n0 = c(1, 2),
          roc = c(0.1, 0.2)
        ),
        tibble::tibble(
          age = c(0, 500),
          n0 = c(3, 4),
          roc = c(0.3, 0.4)
        ),
        tibble::tibble(
          age = c(0, 500),
          n0 = c(5, 6),
          roc = c(0.5, 0.6)
        )
      )
    )

  data_meta <-
    tibble::tibble(
      dataset_id = c("d1", "d2", "d3"),
      region = c("Europe", "Europe", "Africa"),
      climatezone = c("Temperate", "Temperate", "Tropical")
    )

  result <-
    prepare_pap_model_data(
      data_properties = data_properties,
      data_meta = data_meta,
      pap_vars = c("n0", "roc"),
      age_from = 0,
      age_to = 500,
      min_records = 2
    )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(
    all(
      c(
        "region",
        "climatezone",
        "stratum",
        "dataset_id",
        "age",
        "age_ka",
        "variable",
        "value",
        "n_records"
      ) %in% names(result)
    )
  )
  testthat::expect_identical(unique(as.character(result[["region"]])), "Europe")
  testthat::expect_setequal(unique(result[["variable"]]), c("n0", "roc"))
  testthat::expect_true(all(result[["n_records"]] == 2))
})

testthat::test_that("prepare_pap_model_data() filters sparse strata", {
  data_properties <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      data_merge = list(
        tibble::tibble(age = 0, n0 = 1),
        tibble::tibble(age = 0, n0 = 2)
      )
    )

  data_meta <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      region = c("Europe", "Asia"),
      climatezone = c("Temperate", "Cold")
    )

  result <-
    prepare_pap_model_data(
      data_properties = data_properties,
      data_meta = data_meta,
      pap_vars = "n0",
      min_records = 2
    )

  testthat::expect_identical(nrow(result), 0L)
})
