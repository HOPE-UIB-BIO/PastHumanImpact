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
  testthat::expect_true(all(result[["adapt_delta"]] == 0.9))
  testthat::expect_true(all(result[["max_treedepth"]] == 10))
  testthat::expect_true("last_run_rhat_q90" %in% names(result))
  testthat::expect_true("last_run_neff_ratio_min" %in% names(result))
})

testthat::test_that("create_model_config_table() applies large-model sampling settings", {
  data_model <-
    tibble::tibble(
      dataset_id = paste0("d", seq_len(3)),
      region = "Europe",
      climatezone = "Temperate",
      stratum = "Europe__Temperate",
      variable = "n0"
    )

  result <-
    create_model_config_table(
      data_model = data_model,
      family_key = c(n0 = "gamma_log"),
      large_model_min_records = 3,
      large_model_total_iterations = 6400,
      large_model_adapt_delta = 0.95,
      large_model_max_treedepth = 12
    )

  testthat::expect_identical(result[["total_iterations"]], 6400)
  testthat::expect_identical(result[["adapt_delta"]], 0.95)
  testthat::expect_identical(result[["max_treedepth"]], 12)
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
