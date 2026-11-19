testthat::test_that("prepare_age_detrended_predictors() removes age trend", {
  set.seed(900723)

  data_nested <-
    tibble::tibble(
      dataset_id = c("a", "b"),
      data_merge = list(
        tibble::tibble(
          age = seq(2000, 8000, by = 500),
          proxy = 0.3 + 1e-4 * seq(2000, 8000, by = 500) + stats::rnorm(13)
        ),
        tibble::tibble(
          age = seq(2000, 6500, by = 500),
          proxy = exp(seq(2000, 6500, by = 500) / 2000)
        )
      )
    )

  result <-
    prepare_age_detrended_predictors(
      data_source = data_nested,
      predictor_columns = "proxy"
    )

  purrr::walk(
    dplyr::pull(result, "data_merge"),
    .f = ~ {
      vec_detrended <- dplyr::pull(.x, "proxy_age_detrended")
      vec_age <- dplyr::pull(.x, "age")
      testthat::expect_lt(abs(stats::cor(vec_detrended, vec_age)), 1e-10)
      testthat::expect_lt(abs(mean(vec_detrended)), 1e-10)
    }
  )
})

testthat::test_that("prepare_age_detrended_predictors() keeps inputs", {
  data_unit <-
    tibble::tibble(
      age = c(8000, 2000, 5000, 3000),
      proxy = c(4, 1, 3, 1.5),
      other = c("x", "y", "z", "w")
    )

  data_nested <-
    tibble::tibble(dataset_id = "a", data_merge = list(data_unit))

  result <-
    prepare_age_detrended_predictors(
      data_source = data_nested,
      predictor_columns = "proxy"
    )

  data_out <-
    dplyr::pull(result, "data_merge")[[1]]

  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_named(
    data_out,
    c("age", "proxy", "other", "proxy_age_detrended")
  )
  testthat::expect_identical(dplyr::pull(data_out, "proxy"), c(4, 1, 3, 1.5))
  testthat::expect_identical(
    dplyr::pull(data_out, "other"),
    c("x", "y", "z", "w")
  )

  vec_expected <-
    stats::residuals(stats::lm(proxy ~ age, data = data_unit))

  testthat::expect_equal(
    dplyr::pull(data_out, "proxy_age_detrended"),
    unname(vec_expected),
    tolerance = 1e-10
  )
})

testthat::test_that("prepare_age_detrended_predictors() edge cases", {
  data_nested <-
    tibble::tibble(
      dataset_id = c("constant", "linear", "missing"),
      data_merge = list(
        tibble::tibble(age = c(1, 2, 3, 4), proxy = c(0, 0, 0, 0)),
        tibble::tibble(age = c(1, 2, 3, 4), proxy = c(1.1, 1.2, 1.3, 1.4)),
        tibble::tibble(age = c(1, 2, 3, 4), proxy = c(2, NA, 5, 3))
      )
    )

  result <-
    prepare_age_detrended_predictors(
      data_source = data_nested,
      predictor_columns = "proxy"
    )

  list_out <-
    dplyr::pull(result, "data_merge") |>
    purrr::map(~ dplyr::pull(.x, "proxy_age_detrended"))

  testthat::expect_identical(list_out[[1]], c(0, 0, 0, 0))
  testthat::expect_identical(list_out[[2]], c(0, 0, 0, 0))
  testthat::expect_true(is.na(list_out[[3]][[2]]))
  testthat::expect_lt(
    abs(stats::cor(list_out[[3]][-2], c(1, 3, 4))),
    1e-10
  )
})

testthat::test_that("prepare_age_detrended_predictors() validates input", {
  data_nested <-
    tibble::tibble(
      dataset_id = "a",
      data_merge = list(tibble::tibble(age = 1:4, proxy = c(1, 3, 2, 4)))
    )

  testthat::expect_error(
    prepare_age_detrended_predictors(
      data_source = data_nested,
      predictor_columns = "absent"
    )
  )
  testthat::expect_error(
    prepare_age_detrended_predictors(
      data_source = data_nested,
      predictor_columns = 1
    )
  )
  testthat::expect_error(
    prepare_age_detrended_predictors(
      data_source = data_nested,
      predictor_columns = "proxy",
      suffix = ""
    )
  )

  data_existing <-
    tibble::tibble(
      dataset_id = "a",
      data_merge = list(tibble::tibble(
        age = 1:4, proxy = c(1, 3, 2, 4), proxy_age_detrended = 0
      ))
    )

  testthat::expect_error(
    prepare_age_detrended_predictors(
      data_source = data_existing,
      predictor_columns = "proxy"
    )
  )
})
