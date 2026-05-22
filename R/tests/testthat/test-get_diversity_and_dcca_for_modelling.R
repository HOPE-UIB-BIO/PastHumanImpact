testthat::test_that("get_diversity_and_dcca_for_modelling() builds weighted nested series", {
  data_source_diversity <-
    data.frame(
      PAP_diversity = I(list(data.frame(
        dataset_id = c(1, 1),
        sample_id = c("s1", "s2"),
        diversity = c(5, 7)
      ))),
      stringsAsFactors = FALSE
    )

  data_source_dcca <-
    data.frame(
      dcca_scores = I(list(data.frame(
        dataset_id = c(1, 1),
        sample_id = c("s1", "s2"),
        axis_1 = c(0.1, 0.2)
      ))),
      stringsAsFactors = FALSE
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 1,
      levels = I(list(data.frame(
        sample_id = c("s1", "s2"),
        age = c(100, 200),
        lower = c(95, 180),
        upper = c(105, 200)
      ))),
      stringsAsFactors = FALSE
    )

  result <-
    get_diversity_and_dcca_for_modelling(
      data_source_diversity = data_source_diversity,
      data_source_dcca = data_source_dcca,
      data_source_pollen = data_source_pollen
    )

  vec_var_name <-
    result[["var_name"]]

  data_diversity <-
    purrr::pluck(result, "data_to_fit", 1)

  data_dcca <-
    purrr::pluck(result, "data_to_fit", 2)

  testthat::expect_identical(vec_var_name, c("diversity", "dcca_axis_1"))
  testthat::expect_equal(
    as.data.frame(data_diversity),
    data.frame(
      dataset_id = c(1, 1),
      age = c(100, 200),
      value = c(5, 7),
      var_weight = c(1.5, 0.75)
    )
  )
  testthat::expect_equal(
    as.data.frame(data_dcca),
    data.frame(
      dataset_id = c(1, 1),
      age = c(100, 200),
      value = c(0.1, 0.2),
      var_weight = c(1.5, 0.75)
    )
  )
})

testthat::test_that("get_diversity_and_dcca_for_modelling() includes both variables when present", {
  data_source_diversity <-
    data.frame(
      PAP_diversity = I(list(data.frame(
        dataset_id = c(2, 2),
        sample_id = c("a", "b"),
        diversity = c(4, 6)
      ))),
      stringsAsFactors = FALSE
    )

  data_source_dcca <-
    data.frame(
      dcca_scores = I(list(data.frame(
        dataset_id = c(2, 2),
        sample_id = c("a", "b"),
        axis_1 = c(0.3, 0.5)
      ))),
      stringsAsFactors = FALSE
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 2,
      levels = I(list(data.frame(
        sample_id = c("a", "b"),
        age = c(500, 600),
        lower = c(490, 580),
        upper = c(510, 610)
      ))),
      stringsAsFactors = FALSE
    )

  result <-
    get_diversity_and_dcca_for_modelling(
      data_source_diversity = data_source_diversity,
      data_source_dcca = data_source_dcca,
      data_source_pollen = data_source_pollen
    )

  testthat::expect_identical(
    sort(dplyr::pull(result, var_name)),
    c("dcca_axis_1", "diversity")
  )
  testthat::expect_true(
    all(purrr::map_lgl(dplyr::pull(result, data_to_fit), ~ all(c("dataset_id", "age", "value", "var_weight") %in% names(.x))))
  )
})

testthat::test_that("get_diversity_and_dcca_for_modelling() drops NA values after pivot", {
  data_source_diversity <-
    data.frame(
      PAP_diversity = I(list(data.frame(
        dataset_id = c(1, 1),
        sample_id = c("s1", "s2"),
        diversity = c(5, NA)
      ))),
      stringsAsFactors = FALSE
    )

  data_source_dcca <-
    data.frame(
      dcca_scores = I(list(data.frame(
        dataset_id = c(1, 1),
        sample_id = c("s1", "s2"),
        axis_1 = c(0.1, 0.2)
      ))),
      stringsAsFactors = FALSE
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 1,
      levels = I(list(data.frame(
        sample_id = c("s1", "s2"),
        age = c(100, 200),
        lower = c(90, 180),
        upper = c(110, 210)
      ))),
      stringsAsFactors = FALSE
    )

  result <-
    get_diversity_and_dcca_for_modelling(
      data_source_diversity = data_source_diversity,
      data_source_dcca = data_source_dcca,
      data_source_pollen = data_source_pollen
    )

  data_div <-
    dplyr::filter(result, var_name == "diversity") |>
    dplyr::pull(data_to_fit) |>
    .subset2(1L)

  testthat::expect_equal(nrow(data_div), 1L)
  testthat::expect_identical(dplyr::pull(data_div, age), 100)
})

testthat::test_that("get_diversity_and_dcca_for_modelling() validates PAP_diversity input", {
  bad_diversity <-
    data.frame(
      dataset_id = 1,
      stringsAsFactors = FALSE
    )

  data_source_dcca <-
    data.frame(
      dcca_scores = I(list(data.frame(
        dataset_id = 1,
        sample_id = "s1",
        axis_1 = 0.2
      ))),
      stringsAsFactors = FALSE
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 1,
      levels = I(list(data.frame(
        sample_id = "s1",
        age = 100,
        lower = 90,
        upper = 110
      ))),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_diversity_and_dcca_for_modelling(
      data_source_diversity = bad_diversity,
      data_source_dcca = data_source_dcca,
      data_source_pollen = data_source_pollen
    ),
    regexp = "PAP_diversity"
  )
})