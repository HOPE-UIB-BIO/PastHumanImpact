testthat::test_that(
  "resolve_pipeline_profile() returns the profile of the active store",
  {
    contracts <-
      tibble::tibble(
        pipeline_id = c("joint", "single", "other"),
        script = c("R/a/pipeline.R", "R/a/pipeline.R", "R/b/pipeline.R"),
        store_relative_path = c("sens/joint", "sens/single", "sens/other"),
        script_profile = c("joint", "single", NA_character_)
      )

    res <-
      resolve_pipeline_profile(
        data_contracts = contracts,
        script = "R/a/pipeline.R",
        store = "D:/Data/Targets_data/sens/single",
        data_storage_path = "D:/Data",
        default_profile = "joint"
      )

    testthat::expect_type(res, "character")

    testthat::expect_length(res, 1L)

    testthat::expect_equal(res, "single")
  }
)

testthat::test_that(
  "resolve_pipeline_profile() ignores separators and trailing slashes",
  {
    contracts <-
      tibble::tibble(
        pipeline_id = c("joint", "single"),
        script = c("R/a/pipeline.R", "R/a/pipeline.R"),
        store_relative_path = c("sens/joint", "sens/single"),
        script_profile = c("joint", "single")
      )

    res <-
      resolve_pipeline_profile(
        data_contracts = contracts,
        script = "R/a/pipeline.R",
        store = "D:\\Data\\Targets_data\\sens\\single\\",
        data_storage_path = "D:/Data/",
        default_profile = "joint"
      )

    testthat::expect_equal(res, "single")
  }
)

testthat::test_that(
  "resolve_pipeline_profile() falls back when no store matches",
  {
    contracts <-
      tibble::tibble(
        pipeline_id = c("joint", "single", "other"),
        script = c("R/a/pipeline.R", "R/a/pipeline.R", "R/b/pipeline.R"),
        store_relative_path = c("sens/joint", "sens/single", "sens/other"),
        script_profile = c("joint", "single", "other")
      )

    res_unregistered <-
      resolve_pipeline_profile(
        data_contracts = contracts,
        script = "R/a/pipeline.R",
        store = "_targets",
        data_storage_path = "D:/Data",
        default_profile = "joint"
      )

    testthat::expect_equal(res_unregistered, "joint")

    res_other_script <-
      resolve_pipeline_profile(
        data_contracts = contracts,
        script = "R/a/pipeline.R",
        store = "D:/Data/Targets_data/sens/other",
        data_storage_path = "D:/Data",
        default_profile = "joint"
      )

    testthat::expect_equal(res_other_script, "joint")
  }
)

testthat::test_that(
  "resolve_pipeline_profile() falls back for a matched empty profile",
  {
    contracts <-
      tibble::tibble(
        pipeline_id = "single",
        script = "R/a/pipeline.R",
        store_relative_path = "sens/single",
        script_profile = NA_character_
      )

    res <-
      resolve_pipeline_profile(
        data_contracts = contracts,
        script = "R/a/pipeline.R",
        store = "D:/Data/Targets_data/sens/single",
        data_storage_path = "D:/Data",
        default_profile = "joint"
      )

    testthat::expect_equal(res, "joint")
  }
)

testthat::test_that(
  "resolve_pipeline_profile() validates its inputs",
  {
    contracts <-
      tibble::tibble(
        pipeline_id = "single",
        script = "R/a/pipeline.R",
        store_relative_path = "sens/single",
        script_profile = "single"
      )

    testthat::expect_error(
      resolve_pipeline_profile(
        data_contracts = base::as.list(contracts),
        script = "R/a/pipeline.R",
        store = "D:/Data/Targets_data/sens/single",
        data_storage_path = "D:/Data",
        default_profile = "joint"
      ),
      regexp = "contract"
    )

    testthat::expect_error(
      resolve_pipeline_profile(
        data_contracts = dplyr::select(contracts, -"script_profile"),
        script = "R/a/pipeline.R",
        store = "D:/Data/Targets_data/sens/single",
        data_storage_path = "D:/Data",
        default_profile = "joint"
      ),
      regexp = "contract"
    )

    testthat::expect_error(
      resolve_pipeline_profile(
        data_contracts = contracts,
        script = c("R/a/pipeline.R", "R/b/pipeline.R"),
        store = "D:/Data/Targets_data/sens/single",
        data_storage_path = "D:/Data",
        default_profile = "joint"
      ),
      regexp = "contract"
    )

    testthat::expect_error(
      resolve_pipeline_profile(
        data_contracts = contracts,
        script = "R/a/pipeline.R",
        store = NA_character_,
        data_storage_path = "D:/Data",
        default_profile = "joint"
      ),
      regexp = "contract"
    )

    testthat::expect_error(
      resolve_pipeline_profile(
        data_contracts = contracts,
        script = "R/a/pipeline.R",
        store = "D:/Data/Targets_data/sens/single",
        data_storage_path = "D:/Data",
        default_profile = ""
      ),
      regexp = "contract"
    )
  }
)

testthat::test_that(
  "resolve_pipeline_profile() resolves the registered human-proxy stores",
  {
    contracts <-
      readr::read_csv(
        here::here(
          "R", "analyses", "00_profiles", "pipeline_contracts.csv"
        ),
        show_col_types = FALSE
      )

    script <-
      base::file.path(
        "R", "analyses", "91_sensitivity_analyses",
        "joint_human_proxy_hvarpart", "pipeline.R"
      )

    vec_stores <-
      c(
        joint_human_proxy_hvarpart = "spd_kk10_hyde",
        kk10_only_hvarpart = "kk10",
        hyde_only_hvarpart = "hyde",
        spd_kk10_hvarpart = "spd_kk10"
      )

    res <-
      base::names(vec_stores) |>
      purrr::map_chr(
        .f = ~ resolve_pipeline_profile(
          data_contracts = contracts,
          script = script,
          store = base::file.path(
            "D:/Data", "Targets_data", "sensitivity_analyses", .x
          ),
          data_storage_path = "D:/Data",
          default_profile = "unmatched"
        )
      )

    testthat::expect_equal(res, base::unname(vec_stores))
  }
)
