testthat::test_that(
  "temporal configuration preserves, replaces, and deactivates rows",
  {
    current <-
      tibble::tibble(
        model_id = c("same", "changed", "removed"),
        specification_hash = c("a", "old", "gone"),
        need_to_run = c(FALSE, FALSE, FALSE),
        need_to_be_evaluated = c(FALSE, FALSE, FALSE),
        prediction_written = c(TRUE, TRUE, TRUE),
        is_active_model = TRUE
      )

    candidate <-
      tibble::tibble(
        model_id = c("same", "changed", "new"),
        specification_hash = c("a", "new", "fresh"),
        need_to_run = c(TRUE, TRUE, TRUE),
        need_to_be_evaluated = c(FALSE, FALSE, FALSE),
        prediction_written = c(FALSE, FALSE, FALSE),
        is_active_model = TRUE
      )

    result <-
      reconcile_temporal_model_configuration(
        data_current = current,
        data_candidate = candidate
      )

    same <- dplyr::filter(result, .data[["model_id"]] == "same")
    changed <- dplyr::filter(result, .data[["model_id"]] == "changed")
    removed <- dplyr::filter(result, .data[["model_id"]] == "removed")

    testthat::expect_false(same[["need_to_run"]])
    testthat::expect_true(same[["prediction_written"]])
    testthat::expect_true(changed[["need_to_run"]])
    testthat::expect_false(changed[["prediction_written"]])
    testthat::expect_false(removed[["is_active_model"]])
    testthat::expect_false(removed[["need_to_run"]])
  }
)
