testthat::test_that("get_data_properties() merges nested properties", {
  data_source_diversity <-
    data.frame(
      dataset_id = 1,
      data = I(list(data.frame(age = c(1, 2), diversity = c(10, 20))))
    )

  data_source_roc <-
    data.frame(
      dataset_id = 1,
      data = I(list(data.frame(age = c(1, 2), roc = c(0.1, 0.2))))
    )

  data_source_density <-
    data.frame(
      dataset_id = 1,
      pap_density_rescale = I(list(data.frame(age = c(1, 2), density = c(5, 6))))
    )

  res_data <-
    get_data_properties(
      data_source_diversity = data_source_diversity,
      data_source_roc = data_source_roc,
      data_source_density = data_source_density,
      used_rescale = TRUE
    )

  testthat::expect_identical(dplyr::pull(res_data, dataset_id), 1)
  testthat::expect_identical(
    purrr::pluck(res_data, "data_merge", 1),
    data.frame(
      age = c(1, 2),
      diversity = c(10, 20),
      roc = c(0.1, 0.2),
      density = c(5, 6)
    )
  )
})

testthat::test_that("get_data_properties() uses pap_density when used_rescale is FALSE", {
  data_source_diversity <-
    data.frame(
      dataset_id = 5,
      data = I(list(data.frame(age = c(10, 20), diversity = c(1, 2))))
    )

  data_source_roc <-
    data.frame(
      dataset_id = 5,
      data = I(list(data.frame(age = c(10, 20), roc = c(0.2, 0.3))))
    )

  data_source_density <-
    data.frame(
      dataset_id = 5,
      pap_density = I(list(data.frame(age = c(10, 20), density = c(9, 8)))),
      pap_density_rescale = I(list(data.frame(age = c(10, 20), density = c(99, 88))))
    )

  res_data <-
    get_data_properties(
      data_source_diversity = data_source_diversity,
      data_source_roc = data_source_roc,
      data_source_density = data_source_density,
      used_rescale = FALSE
    )

  data_merge <- purrr::pluck(res_data, "data_merge", 1)
  testthat::expect_identical(dplyr::pull(data_merge, density), c(9, 8))
})

testthat::test_that("get_data_properties() drops rows with NA after merge", {
  data_source_diversity <-
    data.frame(
      dataset_id = 2,
      data = I(list(data.frame(age = c(1, 2), diversity = c(10, NA))))
    )

  data_source_roc <-
    data.frame(
      dataset_id = 2,
      data = I(list(data.frame(age = c(1, 2), roc = c(0.1, 0.2))))
    )

  data_source_density <-
    data.frame(
      dataset_id = 2,
      pap_density_rescale = I(list(data.frame(age = c(1, 2), density = c(5, 6))))
    )

  res_data <-
    get_data_properties(
      data_source_diversity = data_source_diversity,
      data_source_roc = data_source_roc,
      data_source_density = data_source_density,
      used_rescale = TRUE
    )

  data_merge <- purrr::pluck(res_data, "data_merge", 1)
  testthat::expect_equal(nrow(data_merge), 1L)
  testthat::expect_identical(dplyr::pull(data_merge, age), 1)
})