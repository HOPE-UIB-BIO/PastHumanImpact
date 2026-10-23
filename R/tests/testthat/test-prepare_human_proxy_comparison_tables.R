testthat::test_that("human-proxy comparison tables retain eligible results", {
  temporal <- list(
    status = tibble::tibble(
      model_id = "joint_filtered", dataset_id = "a",
      analysis = "time", status = "estimated"
    ),
    components = dplyr::bind_rows(
      tibble::tibble(
        model_id = "joint_filtered", dataset_id = "a", analysis = "time",
        predictor = c("human", "climate", "time"),
        individual = c(0.2, 0.1, 0.1),
        model_profile = "human_climate_time",
        total_adjusted_r_squared = 0.4
      ),
      tibble::tibble(
        model_id = "joint_filtered", dataset_id = "a", analysis = "time",
        predictor = c("human", "climate"), individual = c(0.25, 0.15),
        model_profile = "human_climate", total_adjusted_r_squared = 0.4
      )
    )
  )
  spatial <- list(
    status = tibble::tibble(
      model_id = "joint_filtered", analysis = "space", region = "Europe",
      age = 2000, status = "no_spatial_terms_selected",
      selection_status = "no_eligible_network"
    ),
    components = tibble::tibble(
      model_id = "joint_filtered", analysis = "space", region = "Europe",
      age = 2000, model_profile = "human_climate",
      predictor = c("human", "climate"), individual = c(0.3, 0.2)
    )
  )
  metadata <- tibble::tibble(
    dataset_id = "a", long = 10, lat = 50,
    region = "Europe", climatezone = "Cold"
  )
  result <- prepare_human_proxy_comparison_tables(
    temporal, spatial, metadata
  )
  testthat::expect_identical(result[["temporal_common_ids"]], "a")
  testthat::expect_equal(nrow(result[["composition_common"]]), 3L)
  stack_sum <- sum(dplyr::pull(result[["composition_common"]], allocation))
  testthat::expect_equal(stack_sum, 1)
  testthat::expect_error(
    prepare_human_proxy_comparison_tables(list(), spatial, metadata),
    "contract"
  )
})
