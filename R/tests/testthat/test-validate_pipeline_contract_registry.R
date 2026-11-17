testthat::test_that(
  "pipeline contracts require unique ownership and public targets",
  {
    path_script <- tempfile(fileext = ".R")

    writeLines("list()", path_script)

    contracts <-
      tibble::tibble(
        pipeline_id = "pipeline",
        script = path_script,
        store_relative_path = "analysis/pipeline",
        public_targets = "result",
        runner = path_script
      )

    testthat::expect_silent(
      validate_pipeline_contract_registry(contracts)
    )

    duplicated <- dplyr::bind_rows(contracts, contracts)

    testthat::expect_error(
      validate_pipeline_contract_registry(duplicated),
      regexp = "invalid"
    )
  }
)

testthat::test_that(
  "a script is shared only by rows with distinct declared profiles",
  {
    path_script <- tempfile(fileext = ".R")

    writeLines("list()", path_script)

    contracts_shared <-
      tibble::tibble(
        pipeline_id = c("joint", "single"),
        script = c(path_script, path_script),
        store_relative_path = c("analysis/joint", "analysis/single"),
        public_targets = c("result", "result"),
        runner = c(path_script, path_script),
        script_profile = c("joint", "single")
      )

    testthat::expect_silent(
      validate_pipeline_contract_registry(contracts_shared)
    )

    contracts_missing_profile <-
      dplyr::mutate(
        contracts_shared,
        script_profile = c("joint", NA_character_)
      )

    testthat::expect_error(
      validate_pipeline_contract_registry(contracts_missing_profile),
      regexp = "profile"
    )

    contracts_same_profile <-
      dplyr::mutate(
        contracts_shared,
        script_profile = c("joint", "joint")
      )

    testthat::expect_error(
      validate_pipeline_contract_registry(contracts_same_profile),
      regexp = "profile"
    )
  }
)

testthat::test_that(
  "declared profiles do not relax store or identifier uniqueness",
  {
    path_script <- tempfile(fileext = ".R")

    writeLines("list()", path_script)

    contracts_same_store <-
      tibble::tibble(
        pipeline_id = c("joint", "single"),
        script = c(path_script, path_script),
        store_relative_path = c("analysis/joint", "analysis/joint"),
        public_targets = c("result", "result"),
        runner = c(path_script, path_script),
        script_profile = c("joint", "single")
      )

    testthat::expect_error(
      validate_pipeline_contract_registry(contracts_same_store),
      regexp = "invalid"
    )
  }
)

testthat::test_that(
  "unshared scripts may leave the profile empty",
  {
    script_first <- tempfile(fileext = ".R")

    script_second <- tempfile(fileext = ".R")

    writeLines("list()", script_first)

    writeLines("list()", script_second)

    contracts <-
      tibble::tibble(
        pipeline_id = c("first", "second"),
        script = c(script_first, script_second),
        store_relative_path = c("analysis/first", "analysis/second"),
        public_targets = c("result", "result"),
        runner = c(script_first, script_second),
        script_profile = c(NA_character_, NA_character_)
      )

    testthat::expect_silent(
      validate_pipeline_contract_registry(contracts)
    )
  }
)

testthat::test_that(
  "the project pipeline contract registry validates",
  {
    contracts <-
      readr::read_csv(
        here::here(
          "R", "analyses", "00_profiles", "pipeline_contracts.csv"
        ),
        show_col_types = FALSE
      )

    testthat::expect_silent(
      withr::with_dir(
        here::here(),
        validate_pipeline_contract_registry(contracts)
      )
    )
  }
)
