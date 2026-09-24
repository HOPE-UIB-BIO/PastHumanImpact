testthat::test_that(
  "prepare_joint_human_proxy_example_trends creates observed plotting rows",
  {
    data_proxy <-
      tidyr::crossing(
        dataset_id = c("d1", "d2"),
        age_bp = c(1500, 2000, 2500)
      ) |>
      dplyr::mutate(
        region = dplyr::if_else(dataset_id == "d1", "Europe", "Asia"),
        spd = age_bp / 1000,
        kk10 = age_bp / 2000,
        hyde = age_bp / 3000
      )
    data_metadata <-
      tibble::tibble(
        dataset_id = c("d1", "d2"),
        region = c("Europe", "Asia"),
        climatezone = c("Temperate", "Cold")
      )

    result <-
      prepare_joint_human_proxy_example_trends(
        data_proxy_matches = data_proxy,
        data_metadata = data_metadata,
        dataset_ids = c("d1", "d2"),
        variables = c("kk10", "hyde"),
        age_min = 2000,
        age_max = 2500
      )

    testthat::expect_s3_class(result, "data.frame")
    testthat::expect_equal(nrow(result), 8L)
    testthat::expect_setequal(result[["variable"]], c("kk10", "hyde"))
    testthat::expect_setequal(result[["variable_label"]], c("KK10", "HYDE"))
    testthat::expect_true(all(dplyr::between(result[["age"]], 2000, 2500)))
    testthat::expect_false(anyNA(result[["value"]]))
  }
)

testthat::test_that(
  "prepare_joint_human_proxy_example_trends rejects duplicate keys",
  {
    data_proxy <-
      tibble::tibble(
        dataset_id = c("d1", "d1"),
        age_bp = c(2000, 2000),
        region = "Europe",
        spd = 1,
        kk10 = 2,
        hyde = 3
      )
    data_metadata <-
      tibble::tibble(
        dataset_id = "d1",
        region = "Europe",
        climatezone = "Temperate"
      )

    testthat::expect_error(
      prepare_joint_human_proxy_example_trends(
        data_proxy_matches = data_proxy,
        data_metadata = data_metadata,
        dataset_ids = "d1"
      ),
      regexp = "unique dataset-age keys"
    )
  }
)

testthat::test_that(
  "prepare_joint_human_proxy_example_trends rejects region disagreement",
  {
    data_proxy <-
      tibble::tibble(
        dataset_id = "d1",
        age_bp = 2000,
        region = "Europe",
        spd = 1,
        kk10 = 2,
        hyde = 3
      )
    data_metadata <-
      tibble::tibble(
        dataset_id = "d1",
        region = "Asia",
        climatezone = "Temperate"
      )

    testthat::expect_error(
      prepare_joint_human_proxy_example_trends(
        data_proxy_matches = data_proxy,
        data_metadata = data_metadata,
        dataset_ids = "d1"
      ),
      regexp = "regions disagree"
    )
  }
)
