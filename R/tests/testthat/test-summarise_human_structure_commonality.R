testthat::test_that("summarise_human_structure_commonality() values", {
  data_commonality <-
    tibble::tibble(
      model_id = "m",
      dataset_id = "a",
      groups = c(
        "human", "climate", "time", "human&climate", "human&time",
        "climate&time", "human&climate&time"
      ),
      fraction = c(0.10, 0.20, 0.05, -0.02, 0.06, 0.03, 0.04),
      total_fraction = 0.46
    )

  result <-
    summarise_human_structure_commonality(data_commonality = data_commonality)

  data_values <-
    result[["dataset_values"]]

  testthat::expect_named(result, c("dataset_values", "summary"))
  testthat::expect_equal(nrow(data_values), 1L)
  testthat::expect_equal(dplyr::pull(data_values, "unique_human"), 0.10)
  testthat::expect_equal(
    dplyr::pull(data_values, "human_structure_two_way"),
    0.06
  )
  testthat::expect_equal(
    dplyr::pull(data_values, "human_shared_with_structure"),
    0.10
  )
  testthat::expect_equal(dplyr::pull(data_values, "human_marginal"), 0.18)
  testthat::expect_equal(
    dplyr::pull(data_values, "shared_share_of_marginal"),
    0.10 / 0.18
  )
})

testthat::test_that("summarise_human_structure_commonality() summary", {
  data_commonality <-
    tibble::tibble(
      model_id = "m",
      dataset_id = rep(c("a", "b", "c"), each = 2L),
      groups = rep(c("human", "human&time"), times = 3L),
      fraction = c(0.1, 0.1, 0.2, 0.0, -0.1, 0.0),
      total_fraction = c(0.2, 0.2, 0.2, 0.2, -0.1, -0.1)
    )

  result <-
    summarise_human_structure_commonality(data_commonality = data_commonality)

  data_summary <-
    result[["summary"]]

  testthat::expect_equal(nrow(data_summary), 1L)
  testthat::expect_equal(dplyr::pull(data_summary, "n_records"), 3L)
  testthat::expect_equal(
    dplyr::pull(data_summary, "human_structure_two_way_median"),
    0
  )
  testthat::expect_true(is.na(
    dplyr::pull(result[["dataset_values"]], "shared_share_of_marginal")[[3]]
  ))
})

testthat::test_that("summarise_human_structure_commonality() validates", {
  testthat::expect_error(
    summarise_human_structure_commonality(
      data_commonality = tibble::tibble(model_id = "m")
    )
  )
  testthat::expect_error(
    summarise_human_structure_commonality(
      data_commonality = tibble::tibble(
        model_id = "m", dataset_id = "a", groups = "human",
        fraction = 0.1, total_fraction = 0.1
      ),
      summary_columns = "dataset_id_absent"
    )
  )
})
