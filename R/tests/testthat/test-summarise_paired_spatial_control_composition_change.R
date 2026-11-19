build_paired_composition_fixture <- function() {
  res <-
    tidyr::expand_grid(
      model_id = c("ref", "cmp"),
      region = "Europe",
      age = c(2000, 2500, 3000),
      predictor = c("human", "climate", "space")
    ) |>
    dplyr::mutate(
      allocation = dplyr::case_when(
        .data[["predictor"]] == "space" ~ 0.2,
        .data[["model_id"]] == "ref" & .data[["predictor"]] == "human" ~ 0.5,
        .data[["model_id"]] == "ref" ~ 0.3,
        .data[["predictor"]] == "human" ~ 0.3,
        .default = 0.5
      ),
      individual = .data[["allocation"]] / 10
    ) |>
    dplyr::filter(
      !(.data[["model_id"]] == "cmp" & .data[["age"]] == 3000)
    )

  return(res)
}

testthat::test_that("summarise_paired_spatial_control_composition_change()", {
  result <-
    summarise_paired_spatial_control_composition_change(
      data_composition = build_paired_composition_fixture(),
      reference_model_id = "ref",
      comparison_model_id = "cmp"
    )

  testthat::expect_named(result, c("unit_pairs", "summary", "exclusions"))
  testthat::expect_equal(nrow(result[["unit_pairs"]]), 2L)
  testthat::expect_equal(
    dplyr::pull(result[["unit_pairs"]], "change_zero_difference"),
    c(-0.4, -0.4)
  )
  testthat::expect_equal(nrow(result[["exclusions"]]), 1L)
  testthat::expect_identical(
    dplyr::pull(result[["exclusions"]], "present_in"),
    "ref"
  )

  data_zero <-
    result[["summary"]] |>
    dplyr::filter(.data[["profile"]] == "zero_truncated")

  testthat::expect_equal(dplyr::pull(data_zero, "n_units"), 2L)
  testthat::expect_equal(dplyr::pull(data_zero, "reference_mean"), 0.2)
  testthat::expect_equal(dplyr::pull(data_zero, "comparison_median"), -0.2)
  testthat::expect_equal(dplyr::pull(data_zero, "reference_n_human"), 2L)
  testthat::expect_equal(dplyr::pull(data_zero, "comparison_n_human"), 0L)
  testthat::expect_equal(dplyr::pull(data_zero, "n_ranking_changed"), 2L)

  data_signed <-
    result[["summary"]] |>
    dplyr::filter(.data[["profile"]] == "signed")

  testthat::expect_equal(dplyr::pull(data_signed, "change_mean"), -0.04)
})

testthat::test_that("paired composition change validates inputs", {
  testthat::expect_error(
    summarise_paired_spatial_control_composition_change(
      data_composition = dplyr::select(
        build_paired_composition_fixture(),
        -"allocation"
      ),
      reference_model_id = "ref",
      comparison_model_id = "cmp"
    )
  )
  testthat::expect_error(
    summarise_paired_spatial_control_composition_change(
      data_composition = build_paired_composition_fixture(),
      reference_model_id = "ref",
      comparison_model_id = "absent"
    )
  )
})
