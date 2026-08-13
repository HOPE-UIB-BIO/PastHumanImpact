testthat::test_that("compute_next_model_seed() updates selected models", {
  data_config <-
    tibble::tibble(
      model_id = c("model_a", "model_b"),
      seed_base = c(1234L, 1234L),
      seed_attempt = c(1L, 2L),
      sampling_seed = resolve_model_seed(
        model_id = c("model_a", "model_b"),
        seed_attempt = c(1L, 2L),
        seed_base = c(1234L, 1234L)
      ),
      seed_change_reason = "initial_model_seed"
    )

  result <-
    compute_next_model_seed(
      data_config = data_config,
      model_ids = "model_b",
      reason = "sampler_diagnostics_failed"
    )

  testthat::expect_identical(result[["seed_attempt"]], c(1L, 3L))
  testthat::expect_identical(
    result[["sampling_seed"]][1],
    data_config[["sampling_seed"]][1]
  )
  testthat::expect_false(
    result[["sampling_seed"]][2] == data_config[["sampling_seed"]][2]
  )
  testthat::expect_identical(
    result[["seed_change_reason"]][2],
    "sampler_diagnostics_failed"
  )
})

testthat::test_that("compute_next_model_seed() rejects unknown models", {
  data_config <-
    tibble::tibble(
      model_id = "model_a",
      seed_base = 1234L,
      seed_attempt = 1L,
      sampling_seed = 123L,
      seed_change_reason = "initial_model_seed"
    )

  testthat::expect_error(
    compute_next_model_seed(
      data_config = data_config,
      model_ids = "model_b",
      reason = "manual_rerun"
    ),
    regexp = "Unknown"
  )
})
