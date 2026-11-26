testthat::test_that("aggregate_human_proxy_regional() averages records", {
  data_values <-
    tibble::tibble(
      dataset_id = c("a", "b", "c", "d", "e", "a", "b"),
      age_bp = 1000,
      proxy = "spd",
      support = "spd_buffer",
      radius_km = c(250, 500, 250, 250, 250, 250, 500),
      value = c(1, 3, NA, 10, 20, 5, 7),
      value_transformed = base::sqrt(c(1, 3, NA, 10, 20, 5, 7)),
      is_native_layer = TRUE
    ) |>
    dplyr::mutate(
      age_bp = c(1000, 1000, 1000, 1000, 1000, 1500, 1500)
    )

  data_meta <-
    tibble::tibble(
      dataset_id = c("a", "b", "c", "e"),
      region = c("Europe", "Europe", "Asia", "Asia"),
      climatezone = c("Polar", "Polar", "Arid", NA)
    )

  res <-
    aggregate_human_proxy_regional(
      data_values = data_values,
      data_meta = data_meta
    ) |>
    dplyr::arrange(.data[["age_bp"]])

  testthat::expect_named(
    res,
    c(
      "region",
      "climatezone",
      "age_bp",
      "proxy",
      "support",
      "value",
      "n_datasets"
    )
  )
  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_equal(dplyr::pull(res, "value"), c(2, 6))
  testthat::expect_equal(dplyr::pull(res, "n_datasets"), c(2L, 2L))
  testthat::expect_identical(
    dplyr::pull(res, "region"),
    c("Europe", "Europe")
  )
})

testthat::test_that("aggregate_human_proxy_regional() keeps supports apart", {
  data_values <-
    tibble::tibble(
      dataset_id = c("a", "a"),
      age_bp = 500,
      proxy = "kk10",
      support = c("cell", "radius_10km"),
      radius_km = c(NA, 10),
      value = c(0.1, 0.4),
      value_transformed = c(0.1, 0.4),
      is_native_layer = TRUE
    )

  res <-
    aggregate_human_proxy_regional(
      data_values = data_values,
      data_meta = tibble::tibble(
        dataset_id = "a",
        region = "Oceania",
        climatezone = "Tropical"
      )
    ) |>
    dplyr::arrange(.data[["support"]])

  testthat::expect_equal(dplyr::pull(res, "value"), c(0.1, 0.4))
})

testthat::test_that("aggregate_human_proxy_regional() validates inputs", {
  testthat::expect_error(
    aggregate_human_proxy_regional(
      data_values = tibble::tibble(dataset_id = "a", value = 1),
      data_meta = tibble::tibble(
        dataset_id = "a",
        region = "Europe",
        climatezone = "Polar"
      )
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    aggregate_human_proxy_regional(
      data_values = tibble::tibble(
        dataset_id = "a",
        age_bp = 1,
        proxy = "p",
        support = "cell",
        value = 1
      ),
      data_meta = tibble::tibble(
        dataset_id = c("a", "a"),
        region = "Europe",
        climatezone = "Polar"
      )
    ),
    regexp = "contract"
  )
})
