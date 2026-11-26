testthat::test_that("human-proxy layer pipeline contract is registered", {
  contracts <-
    readr::read_csv(
      here::here("R", "analyses", "00_profiles", "pipeline_contracts.csv"),
      show_col_types = FALSE
    )

  contract <-
    contracts |>
    dplyr::filter(.data[["pipeline_id"]] == "human_proxies")

  testthat::expect_equal(base::nrow(contract), 1L)
  testthat::expect_identical(
    dplyr::pull(contract, "script"),
    "R/analyses/01_data_preparation/07_human_proxies/pipeline.R"
  )
  testthat::expect_identical(
    dplyr::pull(contract, "store_relative_path"),
    "data_preparation/human_proxies"
  )
  testthat::expect_identical(
    dplyr::pull(contract, "runner"),
    "R/analyses/01_data_preparation/00_run.R"
  )

  vec_public_targets <-
    dplyr::pull(contract, "public_targets") |>
    stringr::str_split_1(pattern = ";")

  testthat::expect_true(
    base::all(
      c(
        "data_human_proxy_values",
        "data_human_proxy_regional",
        "table_human_proxy_layer_coverage",
        "table_human_proxy_layer_provenance",
        "table_human_proxy_early_holocene_check"
      ) %in% vec_public_targets
    )
  )
})

testthat::test_that("data-preparation runner runs the human-proxy layer", {
  vec_runner_lines <-
    base::readLines(
      here::here("R", "analyses", "01_data_preparation", "00_run.R"),
      warn = FALSE
    )

  testthat::expect_true(
    base::any(
      stringr::str_detect(
        vec_runner_lines,
        stringr::fixed(
          '"07_human_proxies/pipeline.R", "data_preparation/human_proxies"'
        )
      )
    )
  )
})

testthat::test_that("human-proxy layer manifest validates", {
  sources <-
    readr::read_csv(
      here::here(
        "R",
        "analyses",
        "01_data_preparation",
        "07_human_proxies",
        "proxy_sources.csv"
      ),
      show_col_types = FALSE
    )

  res <-
    validate_human_proxy_source_manifest(
      data_sources = sources,
      data_storage_path = base::tempdir(),
      require_files = FALSE,
      expected_source_ids = dplyr::pull(sources, "source_id")
    )

  testthat::expect_true("kk10" %in% dplyr::pull(res, "source_id"))
  testthat::expect_false(base::anyNA(dplyr::pull(res, "file_md5")))
  testthat::expect_false(base::anyNA(dplyr::pull(res, "file_size_bytes")))
})
