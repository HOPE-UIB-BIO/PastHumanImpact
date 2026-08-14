testthat::test_that(
  "compute_dbmem_basis() creates grouped positive spatial predictors",
  {
    data_input <-
      tibble::tibble(
        dataset_id = 1:12,
        long = c(seq(0, 0.5, length.out = 6),
          seq(20, 20.5, length.out = 6)),
        lat = c(rep(45, 6), rep(-20, 6)),
        region = rep(c("north", "south"), each = 6)
      )

    result <-
      compute_dbmem_basis(
        data_source = data_input,
        group_col = "region",
        min_unique_locations = 5L
      )

    testthat::expect_type(result, "list")
    testthat::expect_equal(nrow(result[["basis"]]), 12L)
    testthat::expect_equal(nrow(result[["diagnostics"]]), 2L)
    testthat::expect_true(
      "warning_message" %in% names(result[["diagnostics"]])
    )
    testthat::expect_true(
      any(stringr::str_starts(names(result[["basis"]]), "dbmem_"))
    )
    testthat::expect_true(
      all(dplyr::pull(result[["diagnostics"]], "status") == "eligible")
    )
  }
)

testthat::test_that(
  "compute_dbmem_basis() reports insufficient unique locations and co-location",
  {
    data_input <-
      tibble::tibble(
        dataset_id = 1:4,
        long = c(0, 0, 1, 2),
        lat = c(0, 0, 0, 0),
        region = "one"
      )

    result <-
      compute_dbmem_basis(
        data_source = data_input,
        group_col = "region",
        min_unique_locations = 4L
      )

    testthat::expect_equal(
      dplyr::pull(result[["diagnostics"]], "status"),
      "insufficient_locations"
    )
    testthat::expect_equal(
      dplyr::pull(result[["diagnostics"]], "n_unique_locations"),
      3L
    )
  }
)
