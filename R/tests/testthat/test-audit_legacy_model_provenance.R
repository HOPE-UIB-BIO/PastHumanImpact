testthat::test_that("legacy audit reports missing model files", {
  model_dir <-
    tempfile(pattern = "legacy-models-")
  dir.create(model_dir)

  result <-
    audit_legacy_model_provenance(
      model_id = "missing_model",
      model_dir = model_dir
    )

  testthat::expect_identical(
    result[["model_provenance_status"]],
    "legacy_audit_failed"
  )
  testthat::expect_identical(
    result[["model_audit_reason"]],
    "model_file_not_found"
  )
})

testthat::test_that("legacy audit recovers saved chain seeds", {
  class_name <-
    "PastHumanImpactAuditStanFit"

  if (
    !methods::isClass(class_name)
  ) {
    methods::setClass(
      Class = class_name,
      slots = c(
        stan_args = "list",
        sim = "list"
      )
    )
  }

  stan_fit <-
    methods::new(
      Class = class_name,
      stan_args = list(
        list(seed = 101L),
        list(seed = 202L)
      ),
      sim = list(
        chains = 2L,
        iter = 1000L,
        warmup = 500L
      )
    )
  mod <-
    structure(
      list(
        fit = stan_fit,
        family = list(
          family = "gaussian",
          link = "identity"
        )
      ),
      class = "brmsfit"
    )
  model_dir <-
    tempfile(pattern = "legacy-models-")
  dir.create(model_dir)
  model_dir <-
    normalizePath(
      model_dir,
      winslash = "/"
    )

  RUtilpol::save_latest_file(
    object_to_save = mod,
    file_name = "model_a",
    dir = model_dir,
    prefered_format = "qs",
    verbose = FALSE
  )

  result <-
    audit_legacy_model_provenance(
      model_id = "model_a",
      model_dir = model_dir
    )

  testthat::expect_identical(
    result[["model_provenance_status"]],
    "legacy_seeds_recovered"
  )
  testthat::expect_match(
    result[["model_chain_seeds_json"]],
    '"chain_1":101'
  )
  testthat::expect_identical(result[["saved_model_n_chains"]], 2L)
  testthat::expect_identical(result[["saved_model_family"]], "gaussian")
})
