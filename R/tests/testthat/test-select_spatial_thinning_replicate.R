testthat::test_that(
  "select_spatial_thinning_replicate() preserves separate strata",
  {
    data_input <-
      tibble::tibble(
        dataset_id = c("a", "b", "c"),
        region = c("one", "one", "two")
      )
    mat_distance <-
      matrix(
        c(0, 100, 50, 100, 0, 50, 50, 50, 0),
        nrow = 3,
        byrow = TRUE
      )
    set.seed(900723)

    data_result <-
      select_spatial_thinning_replicate(
        data_coordinates = data_input,
        distance_matrix = mat_distance,
        strata_factor = factor(data_input[["region"]]),
        distance_km = 250,
        repetition = 2L,
        id_col = "dataset_id",
        strata = "region"
      )

    testthat::expect_equal(nrow(data_result), 2L)
    testthat::expect_setequal(
      dplyr::pull(data_result, "region"),
      c("one", "two")
    )
    testthat::expect_true(all(dplyr::pull(data_result, "repetition") == 2L))
  }
)

testthat::test_that(
  "select_spatial_thinning_replicate() validates dimensions",
  {
    testthat::expect_error(
      select_spatial_thinning_replicate(
        data_coordinates = tibble::tibble(
          dataset_id = c("a", "b"),
          region = c("one", "two")
        ),
        distance_matrix = matrix(0, nrow = 3, ncol = 3),
        strata_factor = factor(c("one", "two")),
        distance_km = 250,
        repetition = 1L,
        id_col = "dataset_id",
        strata = "region"
      ),
      "contract"
    )
  }
)
