testthat::test_that("create_model_config_table() creates one row per variable and stratum", {
  data_model <-
    tibble::tibble(
      dataset_id = rep(c("d1", "d1", "d2", "d2"), times = 3),
      region = c(rep("Europe", times = 8), rep("Asia", times = 4)),
      climatezone = c(
        rep("Temperate", times = 8),
        rep("Cold", times = 4)
      ),
      stratum = c(
        rep("Europe__Temperate", times = 8),
        rep("Asia__Cold", times = 4)
      ),
      variable = c(rep("n0", times = 4), rep("roc", times = 4),
        rep("n0", times = 4)),
      age_ka = rep(c(0, 1, 0, 1), times = 3),
      value = c(1, 2, 2, 3, 2, 3, 3, 4, 4, 5, 5, 6)
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
  testthat::expect_true(all(result[["smooth_basis"]] == "cr"))
  testthat::expect_true(all(result[["x_model_var"]] == "age_ka_scaled"))
  testthat::expect_equal(result[["x_mean"]], rep(0.5, times = 3))
  testthat::expect_equal(
    result[["x_sd"]],
    rep(stats::sd(c(0, 1, 0, 1)), times = 3)
  )
  testthat::expect_true(all(result[["is_model_eligible"]]))
  testthat::expect_true(all(result[["model_profile"]] ==
    "within_stratum_dataset_fs"))
  testthat::expect_true(all(stringr::str_detect(
    result[["formula_text"]],
    "s\\(age_ka_scaled, k = 8, bs = 'cr'\\)"
  )))
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
      variable = "n0",
      age_ka = c(0, 1, 2),
      value = c(1, 2, 3)
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
      variable = "n0",
      age_ka = 0,
      value = 1
    )

  testthat::expect_error(
    create_model_config_table(
      data_model = data_model,
      family_key = c(roc = "student_identity")
    ),
    regexp = "cover"
  )
})

testthat::test_that("create_model_config_table() handles response checks", {
  data_model <-
    tibble::tibble(
      dataset_id = rep(rep(c("d1", "d2"), each = 2), times = 2),
      region = "Europe",
      climatezone = "Temperate",
      stratum = "Europe__Temperate",
      variable = rep(c("constant", "between"), each = 4),
      age_ka = rep(c(0, 1, 0, 1), times = 2),
      value = c(rep(1, times = 4), 0, 0, 1, 1)
    )

  result <-
    create_model_config_table(
      data_model = data_model,
      family_key = c(
        constant = "gaussian_identity",
        between = "bernoulli_logit"
      )
    )

  row_constant <-
    result %>%
    dplyr::filter(variable == "constant")
  row_between <-
    result %>%
    dplyr::filter(variable == "between")

  testthat::expect_false(row_constant[["is_model_eligible"]])
  testthat::expect_false(row_constant[["need_to_run"]])
  testthat::expect_identical(
    row_constant[["ineligibility_reason"]],
    "constant_response"
  )
  testthat::expect_true(row_between[["is_model_eligible"]])
  testthat::expect_identical(
    row_between[["model_profile"]],
    "within_stratum_dataset_intercept"
  )
  testthat::expect_identical(
    row_between[["profile_adjustment_reason"]],
    "no_within_dataset_response_variation"
  )
})

testthat::test_that("create_model_config_table() retains partial variation", {
  data_model <-
    tibble::tibble(
      dataset_id = rep(c("d1", "d2"), each = 2),
      region = "Europe",
      climatezone = "Temperate",
      stratum = "Europe__Temperate",
      variable = "event",
      age_ka = c(0, 1, 0, 1),
      value = c(0, 1, 0, 0)
    )

  result <-
    create_model_config_table(
      data_model = data_model,
      family_key = c(event = "bernoulli_logit")
    )

  testthat::expect_identical(
    result[["model_profile"]],
    "within_stratum_dataset_fs"
  )
  testthat::expect_identical(
    result[["datasets_with_response_variation"]],
    1L
  )
})

testthat::test_that("create_model_config_table() rejects invalid scaling", {
  data_model <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      region = "Europe",
      climatezone = "Temperate",
      stratum = "Europe__Temperate",
      variable = "n0",
      age_ka = c(1, 1),
      value = c(1, 2)
    )

  result <-
    create_model_config_table(
      data_model = data_model,
      family_key = c(n0 = "gamma_log")
    )

  testthat::expect_false(result[["is_model_eligible"]])
  testthat::expect_identical(
    result[["ineligibility_reason"]],
    "constant_or_invalid_predictor"
  )

  data_non_finite <-
    data_model %>%
    dplyr::mutate(age_ka = c(0, Inf))

  result_non_finite <-
    suppressWarnings(
      create_model_config_table(
        data_model = data_non_finite,
        family_key = c(n0 = "gamma_log")
      )
    )

  testthat::expect_false(result_non_finite[["is_model_eligible"]])
  testthat::expect_identical(
    result_non_finite[["ineligibility_reason"]],
    "non_finite_predictor"
  )
})
