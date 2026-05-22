testthat::test_that("get_data_filtered removes Africa and private Latin America, applies age range", {
  data_meta <- data.frame(
    dataset_id      = c(1L, 2L, 3L, 4L),
    region          = c("Europe", "Latin America", "Latin America", "Africa"),
    data_publicity  = c("public", "public", "private", "public"),
    stringsAsFactors = FALSE
  )

  age_df <- function(ids) {
    data.frame(age = c(1000L, 5000L, 10000L), val = ids, stringsAsFactors = FALSE)
  }

  data_source <- tibble::tibble(
    dataset_id = c(1L, 2L, 3L, 4L),
    data_merge = list(age_df(1), age_df(2), age_df(3), age_df(4))
  )

  result <- get_data_filtered(
    data_source,
    data_meta,
    age_from = 2000,
    age_to   = 8500,
    remove_private = TRUE
  )

  kept_ids <- result$dataset_id

  # Africa (id=4) and private Latin America (id=3) should be gone
  testthat::expect_true(1L %in% kept_ids)
  testthat::expect_true(2L %in% kept_ids)
  testthat::expect_false(3L %in% kept_ids)
  testthat::expect_false(4L %in% kept_ids)

  # Within remaining datasets, age 1000 and 10000 should be filtered out
  df_europe <-
    dplyr::filter(result, dataset_id == 1L) |>
    dplyr::pull(data_merge) |>
    .subset2(1L)

  testthat::expect_true(
    all(dplyr::pull(df_europe, age) >= 2000 & dplyr::pull(df_europe, age) <= 8500)
  )
})

testthat::test_that("get_data_filtered() keeps private Latin America when remove_private is FALSE", {
  data_meta <-
    data.frame(
      dataset_id = c(1L, 2L),
      region = c("Europe", "Latin America"),
      data_publicity = c("public", "private"),
      stringsAsFactors = FALSE
    )

  data_source <-
    tibble::tibble(
      dataset_id = c(1L, 2L),
      data_merge = list(
        data.frame(age = c(3000L, 4000L), val = 1),
        data.frame(age = c(3000L, 4000L), val = 2)
      )
    )

  res_data <-
    get_data_filtered(
      data_source = data_source,
      data_meta = data_meta,
      age_from = 2000,
      age_to = 8500,
      remove_private = FALSE
    )

  vec_ids <-
    dplyr::pull(res_data, dataset_id)

  testthat::expect_true(all(c(1L, 2L) %in% vec_ids))
})

testthat::test_that("get_data_filtered() validates remove_private input", {
  data_meta <-
    data.frame(
      dataset_id = 1L,
      region = "Europe",
      data_publicity = "public",
      stringsAsFactors = FALSE
    )

  data_source <-
    tibble::tibble(
      dataset_id = 1L,
      data_merge = list(
        data.frame(age = c(3000L, 4000L), val = 1)
      )
    )

  testthat::expect_error(
    get_data_filtered(
      data_source = data_source,
      data_meta = data_meta,
      remove_private = "yes"
    ),
    regexp = "single logical"
  )
})

testthat::test_that("get_data_filtered() returns expected columns and nested structure", {
  data_meta <-
    data.frame(
      dataset_id = c(1L, 2L),
      region = c("Europe", "Latin America"),
      data_publicity = c("public", "public"),
      stringsAsFactors = FALSE
    )

  data_source <-
    tibble::tibble(
      dataset_id = c(1L, 2L),
      data_merge = list(
        data.frame(age = c(2500L, 2600L), val = c(1, 2)),
        data.frame(age = c(2500L, 2600L), val = c(3, 4))
      )
    )

  res_data <-
    get_data_filtered(
      data_source = data_source,
      data_meta = data_meta,
      age_from = 2000,
      age_to = 8500,
      remove_private = TRUE
    )

  testthat::expect_true(all(c("dataset_id", "data_merge") %in% names(res_data)))
  testthat::expect_true(is.list(dplyr::pull(res_data, data_merge)))
})
