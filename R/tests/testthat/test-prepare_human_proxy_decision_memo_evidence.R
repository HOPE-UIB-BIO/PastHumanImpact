testthat::test_that("decision evidence uses exact three-model common cohorts", {
  balance <- tibble::tibble(
    dataset_id = c("a", "b"),
    total_adjusted_r_squared = c(0.4, 0.5),
    human = c(0.1, 0.3), climate = c(0.2, 0.1), time = c(0.1, 0.1),
    signed_difference = c(-0.1, 0.2),
    signed_balance = c(-0.25, 0.4), signed_weight = c(0.4, 0.5),
    zero_balance = c(-1 / 3, 0.5), zero_weight = c(0.3, 0.4),
    region = c("Europe", "Asia"), climatezone = c("Cold", "Temperate"),
    long = c(10, 20), lat = c(50, 40)
  )
  components <- tidyr::crossing(
    dataset_id = c("a", "b"), component = c("human", "climate", "time")
  ) |>
    dplyr::mutate(measure = "unique_adjusted_r2", value = 0.1)
  temporal <- tidyr::crossing(
    analysis = "temporal_spd", region = c("Europe", "Asia"),
    age = 2000, predictor = c("human", "climate", "space")
  ) |>
    dplyr::mutate(
      allocation = dplyr::case_when(
        .data[["predictor"]] == "human" ~ 0.2,
        .data[["predictor"]] == "climate" ~ 0.6,
        TRUE ~ 0.2
      ),
      Unique = 0.1, spatial_adjusted_r_squared = 0.4
    )
  bridge_balance <- dplyr::mutate(
    balance, zero_balance = .data[["zero_balance"]] + 0.1,
    signed_balance = .data[["signed_balance"]] + 0.1
  )
  joint_balance <- dplyr::mutate(
    balance, zero_balance = .data[["zero_balance"]] + 0.5,
    signed_balance = .data[["signed_balance"]] + 0.5
  )
  bridge_temporal <- temporal |>
    dplyr::select(-dplyr::all_of("analysis")) |>
    dplyr::mutate(analysis = "bridge", model_id = "spd_matched_bridge")
  joint_temporal <- temporal |>
    dplyr::select(-dplyr::all_of("analysis")) |>
    dplyr::mutate(analysis = "joint", model_id = "joint_filtered")
  unique_r2 <- tidyr::crossing(
    model_id = c("spd_matched_bridge", "joint_filtered"),
    dataset_id = c("a", "b"),
    fraction = c("pure_human", "pure_climate", "pure_time")
  ) |>
    dplyr::mutate(adjusted_r_squared = 0.1)

  result <- prepare_human_proxy_decision_memo_evidence(
    canonical_balance = balance,
    canonical_components = components,
    canonical_temporal = temporal,
    bridge_comparisons = list(
      balance_common = bridge_balance,
      composition_common = bridge_temporal
    ),
    joint_comparisons = list(
      balance_common = joint_balance,
      composition_common = joint_temporal
    ),
    filtered_temporal_unique = unique_r2
  )

  testthat::expect_equal(nrow(result$spatial_common_cohort), 6L)
  testthat::expect_equal(nrow(result$temporal_common_cohort), 18L)
  testthat::expect_equal(result$spatial_summary$n_sequences, rep(2L, 3L))
  testthat::expect_setequal(
    unique(result$spatial_common_cohort$model_id),
    c("canonical_spd", "spd_matched_bridge", "joint_filtered")
  )
  testthat::expect_true(all(c(
    "continental_region", "region"
  ) %in% names(result$spatial_common_cohort)))
  testthat::expect_setequal(
    unique(result$spatial_plot_records$region),
    c("Europe", "Asia")
  )
  testthat::expect_setequal(
    unique(result$spatial_plot_records$climatezone),
    c("Cold", "Temperate")
  )
  testthat::expect_false(identical(
    result$spatial_plot_records$region,
    result$spatial_plot_records$climatezone
  ))
})
