testthat::test_that(
  "prepare_hvarpart_timebin_groups() creates one element per row",
  {
    data_input <-
      tibble::tibble(
        region = c("Europe", "Asia"),
        age = c(2000, 2000),
        data_merge = list(tibble::tibble(x = 1), tibble::tibble(x = 2))
      )
    result <- prepare_hvarpart_timebin_groups(data_input)

    testthat::expect_length(result, 2L)
    testthat::expect_true(all(purrr::map_int(result, nrow) == 1L))
  }
)
