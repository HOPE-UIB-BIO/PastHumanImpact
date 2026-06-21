testthat::test_that("create_model_config_table() creates one row per variable and stratum", {
  data_model <-
    tibble::tibble(
      dataset_id = c("d1", "d2", "d1", "d2", "d3", "d4"),
      region = c("Europe", "Europe", "Europe", "Europe", "Asia", "Asia"),
      climatezone = c(
        "Temperate",
        "Temperate",
        "Temperate",
        "Temperate",
        "Cold",
        "Cold"
      ),
      stratum = c(
        "Europe__Temperate",
        "Europe__Temperate",
        "Europe__Temperate",
        "Europe__Temperate",
        "Asia__Cold",
        "Asia__Cold"
      ),
      variable = c("n0", "n0", "roc", "roc", "n0", "n0")
    )

  result <-
    create_model_config_table(
      data_model = data_model,
      analysis = "pap_temporal",
      family_key = c(n0 = "student_identity", roc = "student_identity"),
      model_profile = "within_stratum_dataset_fs",
      min_records = 2
    )

  testthat::expect_identical(nrow(result), 3L)
  testthat::expect_setequal(
    result[["model_id"]],
    c(
      "pap_temporal__n0__Asia__Cold",
      "pap_temporal__n0__Europe__Temperate",
      "pap_temporal__roc__Europe__Temperate"
    )
  )
  testthat::expect_identical(result[["output_id"]], result[["model_id"]])
  testthat::expect_true(all(result[["need_to_run"]]))
  testthat::expect_true(all(!result[["need_to_be_evaluated"]]))
  testthat::expect_true(all(result[["n_records"]] == 2))
})

testthat::test_that("create_model_config_table() validates family coverage", {
  data_model <-
    tibble::tibble(
      dataset_id = "d1",
      region = "Europe",
      climatezone = "Temperate",
      stratum = "Europe__Temperate",
      variable = "n0"
    )

  testthat::expect_error(
    create_model_config_table(
      data_model = data_model,
      family_key = c(roc = "student_identity")
    ),
    regexp = "cover"
  )
})
