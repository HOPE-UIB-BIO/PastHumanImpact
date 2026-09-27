testthat::test_that("local selection audits retain unit identifiers", {
  selection <- tibble::tibble(
    group = c("human", "climate"), predictor = c("h", "c"),
    preference_rank = 1L, preselection_status = "candidate",
    selected = TRUE, reason = "retained"
  )
  designs <- tibble::tibble(
    dataset_id = "a", model_id = "joint",
    data_merge = list(tibble::tibble(h = 1:10, c = c(1:5, 7:11))),
    predictor_vars = list(list(human = "h", climate = "c")),
    selection_audit = list(selection)
  )
  result <- summarise_local_predictor_selection_audits(
    designs, "dataset_id", candidate_vars = c("h", "c")
  )
  testthat::expect_true(
    all(c("dataset_id", "model_id") %in% names(result[["selection"]]))
  )
  testthat::expect_equal(nrow(result[["selection"]]), 2L)
  testthat::expect_error(
    summarise_local_predictor_selection_audits(data.frame(x = 1), "x"),
    "contract"
  )
})
