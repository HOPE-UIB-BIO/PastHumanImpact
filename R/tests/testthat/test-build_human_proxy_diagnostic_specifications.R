testthat::test_that("build_human_proxy_diagnostic_specifications() shape", {
  specs <-
    build_human_proxy_diagnostic_specifications()

  testthat::expect_s3_class(specs, "tbl_df")
  testthat::expect_equal(nrow(specs), 7L)
  testthat::expect_named(
    specs,
    c(
      "model_id", "model_label", "human_candidates", "human_preference",
      "exploratory", "proxy", "proxy_variant", "climate_representation",
      "within_dataset_profile_id", "time_slice_profile_id"
    )
  )
  testthat::expect_false(anyDuplicated(dplyr::pull(specs, "model_id")) > 0)
  testthat::expect_false(
    anyDuplicated(dplyr::pull(specs, "within_dataset_profile_id")) > 0
  )
})

testthat::test_that("build_human_proxy_diagnostic_specifications() columns", {
  specs <-
    build_human_proxy_diagnostic_specifications()

  candidates <-
    dplyr::pull(specs, "human_candidates")

  testthat::expect_true(all(purrr::map_int(candidates, length) == 1L))
  testthat::expect_identical(
    candidates,
    dplyr::pull(specs, "human_preference")
  )
  testthat::expect_setequal(
    unlist(candidates),
    c(
      "spd_sqrt", "spd_sqrt_age_detrended",
      "kk10_fraction", "kk10_fraction_age_detrended",
      "hyde_sqrt", "hyde_sqrt_age_detrended"
    )
  )

  detrended <-
    specs |>
    dplyr::filter(.data[["proxy_variant"]] == "age_detrended")

  testthat::expect_true(all(
    stringr::str_detect(unlist(detrended[["human_candidates"]]), "_age_")
  ))
})

testthat::test_that("build_human_proxy_diagnostic_specifications() arms", {
  specs <-
    build_human_proxy_diagnostic_specifications()

  full <-
    specs |>
    dplyr::filter(.data[["climate_representation"]] == "full")

  testthat::expect_equal(dplyr::pull(full, "model_id"), "spd__full_climate")

  slice_ids <-
    specs |>
    dplyr::filter(!is.na(.data[["time_slice_profile_id"]])) |>
    dplyr::pull("model_id")

  testthat::expect_setequal(
    slice_ids,
    c("spd__full_climate", "spd__filtered_climate")
  )
  testthat::expect_true(all(dplyr::pull(specs, "exploratory")))
})
