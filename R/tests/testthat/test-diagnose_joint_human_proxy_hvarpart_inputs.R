testthat::test_that("joint proxy diagnostics recognise the four-df design", {
  set.seed(342)
  temporal <-
    tibble::tibble(
      age = seq(2000, 8000, by = 500),
      response = stats::rnorm(13),
      human_1 = stats::rnorm(13),
      human_2 = stats::rnorm(13),
      human_3 = stats::rnorm(13),
      climate_1 = stats::rnorm(13),
      climate_2 = stats::rnorm(13),
      climate_3 = stats::rnorm(13),
      climate_4 = stats::rnorm(13)
    )
  spatial <-
    temporal[rep(seq_len(nrow(temporal)), length.out = 30), ] |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(
          "response",
          "human_1",
          "human_2",
          "human_3",
          "climate_1",
          "climate_2",
          "climate_3",
          "climate_4"
        )),
        ~ .x + stats::rnorm(length(.x), sd = 0.01)
      )
    ) |>
    dplyr::select(-dplyr::all_of("age"))
  predictors <-
    list(
      human = c("human_1", "human_2", "human_3"),
      climate = c("climate_1", "climate_2", "climate_3", "climate_4")
    )

  result <-
    diagnose_joint_human_proxy_hvarpart_inputs(
      data_within_dataset = tibble::tibble(
        dataset_id = "test",
        data_merge = list(temporal)
      ),
      data_time_slices = tibble::tibble(
        region = "Europe",
        age = 2000,
        data_merge = list(spatial)
      ),
      response_vars = "response",
      predictor_vars = predictors,
      min_temporal_residual_df = 4L
    )

  testthat::expect_identical(
    result[["within_dataset"]][["status"]],
    "estimable"
  )
  testthat::expect_identical(result[["within_dataset"]][["residual_df"]], 4L)
  testthat::expect_true(result[["within_dataset"]][["requested_estimable"]])
  testthat::expect_true(result[["time_slices"]][["design_full_rank"]])
})

testthat::test_that("joint proxy diagnostics retain rank failures", {
  set.seed(343)
  data <-
    tibble::tibble(
      age = seq(2000, 8000, by = 500),
      response = stats::rnorm(13),
      human_1 = stats::rnorm(13),
      human_2 = stats::rnorm(13),
      human_3 = human_2,
      climate_1 = stats::rnorm(13),
      climate_2 = stats::rnorm(13),
      climate_3 = stats::rnorm(13),
      climate_4 = stats::rnorm(13)
    )
  predictors <-
    list(
      human = c("human_1", "human_2", "human_3"),
      climate = c("climate_1", "climate_2", "climate_3", "climate_4")
    )

  result <-
    diagnose_joint_human_proxy_hvarpart_inputs(
      data_within_dataset = tibble::tibble(
        dataset_id = "test",
        data_merge = list(data)
      ),
      data_time_slices = tibble::tibble(
        region = "Europe",
        age = 2000,
        data_merge = list(dplyr::select(data, -dplyr::all_of("age")))
      ),
      response_vars = "response",
      predictor_vars = predictors
    )

  testthat::expect_identical(
    result[["within_dataset"]][["status"]],
    "rank_deficient"
  )
  testthat::expect_identical(result[["time_slices"]][["status"]], "rank_deficient")
})
