make_filter_all_data_fixture <- function() {
  sample_ids <-
    paste0("s", 1:5)

  levels_tbl <-
    data.frame(
      sample_id = sample_ids,
      age = c(0, 100, 200, 300, 400),
      stringsAsFactors = FALSE
    )

  counts_tbl <-
    data.frame(
      sample_id = sample_ids,
      taxon_a = c(200, 210, 220, 230, 240),
      taxon_b = c(10, 10, 10, 10, 10),
      stringsAsFactors = FALSE
    )

  raw_counts_tbl <-
    data.frame(
      sample_id = sample_ids,
      taxon_a = c(100, 105, 110, 115, 120),
      taxon_b = c(5, 5, 5, 5, 5),
      stringsAsFactors = FALSE
    )

  age_uncertainty_mat <-
    matrix(
      data = 0,
      nrow = 2,
      ncol = 5,
      dimnames = list(c("low", "high"), sample_ids)
    )

  data.frame(
    dataset_id = 1,
    levels = I(list(levels_tbl)),
    raw_counts = I(list(raw_counts_tbl)),
    counts_harmonised = I(list(counts_tbl)),
    age_uncertainty = I(list(age_uncertainty_mat)),
    young_age = 50,
    old_age = 350,
    chron_control_limits = I(list(c(0, 400))),
    end_of_interest_period = 350,
    pollen_percentage = FALSE,
    stringsAsFactors = FALSE
  )
}

testthat::test_that("filter_all_data returns filtered data with updated summaries", {
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("assertthat")
  testthat::skip_if_not_installed("tidyselect")
  testthat::skip_if_not_installed("RUtilpol")

  data_source <-
    make_filter_all_data_fixture()

  result <-
    filter_all_data(
      data_source = data_source,
      min_n_grains = 25,
      target_n_grains = 150,
      percentage_samples = 50,
      maximum_age_extrapolation = 3000,
      min_n_levels = 3
    )

  vec_n_sample_counts <-
    result[["n_sample_counts"]]

  vec_age_min <-
    result[["age_min"]]

  vec_age_max <-
    result[["age_max"]]

  data_levels <-
    purrr::pluck(result, "levels", 1)

  data_counts_harmonised <-
    purrr::pluck(result, "counts_harmonised", 1)

  vec_levels_id <-
    data_levels[["sample_id"]]

  vec_counts_id <-
    data_counts_harmonised[["sample_id"]]

  testthat::expect_identical(nrow(result), 1L)
  testthat::expect_identical(vec_n_sample_counts, 4)
  testthat::expect_identical(vec_age_min, 0)
  testthat::expect_identical(vec_age_max, 300)
  testthat::expect_identical(vec_levels_id, vec_counts_id)
})

testthat::test_that("filter_all_data drops sequences below minimum level count", {
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("assertthat")
  testthat::skip_if_not_installed("tidyselect")
  testthat::skip_if_not_installed("RUtilpol")

  data_source <-
    make_filter_all_data_fixture()

  result <-
    filter_all_data(
      data_source = data_source,
      min_n_grains = 25,
      target_n_grains = 150,
      percentage_samples = 50,
      maximum_age_extrapolation = 3000,
      min_n_levels = 5
    )

  testthat::expect_identical(nrow(result), 0L)
})