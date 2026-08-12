testthat::test_that(
  "run_spatial_thinning() is deterministic and respects minimum distance",
  {
    data_input <-
      tibble::tibble(
        dataset_id = 1:8,
        long = c(seq(0, 0.3, by = 0.1), seq(10, 10.3, by = 0.1)),
        lat = 0,
        region = rep(c("a", "b"), each = 4),
        climatezone = "zone"
      )

    result_one <-
      run_spatial_thinning(
        data_source = data_input,
        distance_km = 20,
        repetitions = 3L,
        seed = 900723L
      )
    result_two <-
      run_spatial_thinning(
        data_source = data_input,
        distance_km = 20,
        repetitions = 3L,
        seed = 900723L
      )

    testthat::expect_equal(result_one, result_two)
    testthat::expect_equal(
      sort(unique(dplyr::pull(result_one, "repetition"))),
      1:3
    )

    data_first <-
      result_one |>
      dplyr::filter(.data[["repetition"]] == 1L) |>
      dplyr::left_join(data_input, by = c("dataset_id", "region",
        "climatezone"))
    vec_distance_valid <-
      data_first |>
      dplyr::group_split(.data[["region"]]) |>
      purrr::map_lgl(
        .f = ~ {
          if (
            nrow(.x) <= 1L
          ) {
            return(TRUE)
          }

          mat_distance <-
            get_spatial_distance_matrix(data_source = .x)

          return(all(mat_distance[upper.tri(mat_distance)] >= 20))
        }
      )
    testthat::expect_true(all(vec_distance_valid))
  }
)

testthat::test_that(
  "run_spatial_thinning() validates distances and strata",
  {
    data_input <-
      tibble::tibble(
        dataset_id = 1:2,
        long = c(0, 1),
        lat = c(0, 0),
        region = "a",
        climatezone = "zone"
      )

    testthat::expect_error(
      run_spatial_thinning(
        data_source = data_input,
        distance_km = 0
      ),
      "required contract"
    )
  }
)
