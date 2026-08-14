testthat::test_that("two-key gate selects only authorized requests", {
  data_config <-
    tibble::tibble(
      model_id = c("model_a", "model_b"),
      definition_hash = c("hash_a", "hash_b"),
      is_active_model = TRUE,
      is_model_eligible = TRUE,
      need_to_run = c(TRUE, FALSE)
    )

  data_requests <-
    tibble::tibble(
      request_id = c("request_a", "request_b"),
      model_id = c("model_a", "model_b"),
      definition_hash = c("hash_a", "hash_b"),
      run_requested = TRUE,
      request_reason = "approved",
      requested_at = "2026-08-13"
    )

  result <-
    select_authorized_temporal_model_runs(
      data_config = data_config,
      data_requests = data_requests
    )

  testthat::expect_identical(
    result[["authorized"]][["model_id"]],
    "model_a"
  )
  testthat::expect_identical(
    result[["audit"]][["request_status"]],
    c("authorized", "fit_not_needed")
  )
})

testthat::test_that("consumed and stale requests cannot authorize fits", {
  data_config <-
    tibble::tibble(
      model_id = c("model_a", "model_b"),
      definition_hash = c("hash_a", "hash_b"),
      is_active_model = TRUE,
      is_model_eligible = TRUE,
      need_to_run = TRUE
    )

  data_requests <-
    tibble::tibble(
      request_id = c("request_a", "request_b"),
      model_id = c("model_a", "model_b"),
      definition_hash = c("hash_a", "old_hash"),
      run_requested = TRUE,
      request_reason = "approved",
      requested_at = "2026-08-13"
    )

  data_history <-
    tibble::tibble(
      request_id = "request_a",
      event = "fit_started"
    )

  result <-
    select_authorized_temporal_model_runs(
      data_config = data_config,
      data_requests = data_requests,
      data_history = data_history
    )

  testthat::expect_equal(nrow(result[["authorized"]]), 0L)
  testthat::expect_identical(
    result[["audit"]][["request_status"]],
    c("consumed", "stale_definition")
  )
})
