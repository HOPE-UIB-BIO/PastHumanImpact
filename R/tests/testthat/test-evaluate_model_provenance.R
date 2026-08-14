testthat::test_that("evaluate_model_provenance() updates audited rows", {
  data_config <-
    tibble::tibble(
      model_id = c("model_a", "model_b")
    )
  data_audit <-
    tibble::tibble(
      model_id = "model_b",
      model_file_name = "model_b.qs",
      model_chain_seeds_json = '{"chain_1":202}',
      model_seed_source = "recovered_stan_chain_seeds",
      model_provenance_status = "legacy_seeds_recovered",
      model_audit_reason = NA_character_
    )

  result <-
    evaluate_model_provenance(
      data_config = data_config,
      data_audit = data_audit
    )

  testthat::expect_true(is.na(result[["model_file_name"]][1]))
  testthat::expect_identical(
    result[["model_file_name"]][2],
    "model_b.qs"
  )
  testthat::expect_identical(
    result[["model_provenance_status"]][2],
    "legacy_seeds_recovered"
  )
})

testthat::test_that("evaluate_model_provenance() rejects unknown IDs", {
  data_config <-
    tibble::tibble(model_id = "model_a")
  data_audit <-
    tibble::tibble(
      model_id = "model_b",
      model_file_name = "model_b.qs",
      model_chain_seeds_json = '{"chain_1":202}',
      model_seed_source = "recovered_stan_chain_seeds",
      model_provenance_status = "legacy_seeds_recovered",
      model_audit_reason = NA_character_
    )

  testthat::expect_error(
    evaluate_model_provenance(
      data_config = data_config,
      data_audit = data_audit
    ),
    regexp = "matching model IDs"
  )
})
