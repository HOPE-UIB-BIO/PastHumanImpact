testthat::test_that(
  "select_human_proxy_common_cohort() keeps complete records only",
  {
    data_units <-
      tibble::tibble(
        dataset_id = c("1", "2", "3", "4"),
        data_merge = list(
          tibble::tibble(age = c(2000, 2500), a = c(1, 2), b = c(3, 4)),
          tibble::tibble(age = c(2000, 2500), a = c(1, NA), b = c(3, 4)),
          tibble::tibble(age = c(2000, 2500), a = c(1, 2)),
          tibble::tibble(age = numeric(), a = numeric(), b = numeric())
        )
      )

    res <-
      select_human_proxy_common_cohort(
        data_record_units = data_units,
        required_columns = c("a", "b")
      )

    testthat::expect_named(res, c("cohort", "audit"))
    testthat::expect_equal(dplyr::pull(res[["cohort"]], "dataset_id"), "1")
    testthat::expect_named(
      res[["audit"]],
      c("dataset_id", "n_rows", "is_complete", "incomplete_columns")
    )
    testthat::expect_equal(
      dplyr::pull(res[["audit"]], "is_complete"),
      c(TRUE, FALSE, FALSE, FALSE)
    )
    testthat::expect_equal(
      dplyr::pull(res[["audit"]], "incomplete_columns"),
      c(NA, "a", "b", "a;b")
    )
    testthat::expect_equal(
      dplyr::pull(res[["audit"]], "n_rows"),
      c(2L, 2L, 2L, 0L)
    )
  }
)

testthat::test_that(
  "select_human_proxy_common_cohort() validates its inputs",
  {
    testthat::expect_error(
      select_human_proxy_common_cohort(
        data_record_units = tibble::tibble(dataset_id = "1"),
        required_columns = "a"
      ),
      regexp = "common-cohort inputs"
    )

    testthat::expect_error(
      select_human_proxy_common_cohort(
        data_record_units = tibble::tibble(
          dataset_id = "1",
          data_merge = list(tibble::tibble(a = 1))
        ),
        required_columns = character()
      ),
      regexp = "common-cohort inputs"
    )
  }
)
