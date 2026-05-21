testthat::test_that("get_density_pap_combined() returns expected list-columns", {
  testthat::skip_if_not_installed("REcopol")
  testthat::skip_if_not_installed("scales")

  dummy_time <- data.frame(age = seq(100, 500, by = 100))

  data_cp <- tibble::tibble(
    dataset_id   = 1L,
    diversity_cp = list(data.frame(
      var_name = c("N0", "N0"),
      age      = c(200, 300),
      stringsAsFactors = FALSE
    )),
    mvrt_cp  = list(c(250)),
    roc_cp   = list(c(300)),
    roc_pp   = list(c(350)),
    dcca_cp  = list(c(400))
  )

  data_meta <- tibble::tibble(
    dataset_id = 1L,
    age_min    = 100,
    age_max    = 500
  )

  res_data <-
    get_density_pap_combined(
      data_source_change_points = data_cp,
      data_source_meta          = data_meta,
      data_source_dummy_time    = dummy_time,
      limit_length              = TRUE
    )

  testthat::expect_true(
    all(c("dataset_id", "pap_density", "pap_density_rescale") %in% names(res_data))
  )
  testthat::expect_equal(nrow(res_data), 1L)

  pd <- dplyr::pull(res_data, pap_density_rescale)[[1]]
  testthat::expect_true(is.data.frame(pd) || tibble::is_tibble(pd))
  testthat::expect_true("age" %in% names(pd))
})

testthat::test_that("get_density_pap_combined() respects limit_length age range", {
  testthat::skip_if_not_installed("REcopol")
  testthat::skip_if_not_installed("scales")

  dummy_time <- data.frame(age = seq(100, 700, by = 100))

  data_cp <- tibble::tibble(
    dataset_id   = 2L,
    diversity_cp = list(data.frame(var_name = "N0", age = c(200, 300))),
    mvrt_cp  = list(c(250)),
    roc_cp   = list(c(300)),
    roc_pp   = list(c(350)),
    dcca_cp  = list(c(400))
  )

  data_meta <- tibble::tibble(dataset_id = 2L, age_min = 200, age_max = 500)

  res_data <-
    get_density_pap_combined(
      data_source_change_points = data_cp,
      data_source_meta = data_meta,
      data_source_dummy_time = dummy_time,
      limit_length = TRUE
    )

  pd <- dplyr::pull(res_data, pap_density_rescale)[[1]]
  vec_age <- dplyr::pull(pd, age)
  testthat::expect_true(all(vec_age >= 200 & vec_age <= 500))
})

testthat::test_that("get_density_pap_combined() handles empty change points", {
  testthat::skip_if_not_installed("REcopol")
  testthat::skip_if_not_installed("scales")

  dummy_time <- data.frame(age = seq(100, 300, by = 100))

  data_cp <- tibble::tibble(
    dataset_id   = 3L,
    diversity_cp = list(data.frame(var_name = character(0), age = numeric(0))),
    mvrt_cp  = list(numeric(0)),
    roc_cp   = list(numeric(0)),
    roc_pp   = list(numeric(0)),
    dcca_cp  = list(numeric(0))
  )

  data_meta <- tibble::tibble(dataset_id = 3L, age_min = 100, age_max = 300)

  res_data <-
    get_density_pap_combined(
      data_source_change_points = data_cp,
      data_source_meta = data_meta,
      data_source_dummy_time = dummy_time,
      limit_length = TRUE
    )

  pd <- dplyr::pull(res_data, pap_density)[[1]]
  testthat::expect_true(all(c("age", "density_turnover", "density_diversity") %in% names(pd)))
  testthat::expect_equal(nrow(pd), 3L)
})
