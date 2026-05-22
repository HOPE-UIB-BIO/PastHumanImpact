testthat::test_that("get_scores_dbrda returns NA when input is NULL", {
  result <- get_scores_dbrda(NULL)
  testthat::expect_true(is.na(result))
})

testthat::test_that("get_scores_dbrda extracts tidy scores from a fitted dbrda model", {
  testthat::skip_if_not_installed("vegan")

  set.seed(900723)
  comm <- matrix(abs(stats::rnorm(30)), nrow = 6, ncol = 5)
  rownames(comm) <- paste0("s", 1:6)
  colnames(comm) <- paste0("sp", 1:5)

  pred <- data.frame(x = stats::rnorm(6))

  mod <- vegan::dbrda(comm ~ x, data = pred, distance = "euclidean")
  result <- get_scores_dbrda(mod)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_true("label" %in% names(result))
  testthat::expect_true("score" %in% names(result))
  testthat::expect_true(
    any(grepl("dbRDA|MDS|PC", names(result)))
  )
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("get_scores_dbrda returns tidy scores for score-able objects", {
  testthat::skip_if_not_installed("vegan")

  result <-
    get_scores_dbrda(dbrda_mod = data.frame(x = 1:3))

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_true("label" %in% names(result))
})

testthat::test_that("get_scores_dbrda validates atomic input", {
  testthat::expect_error(
    get_scores_dbrda(dbrda_mod = 42),
    regexp = "model-like object"
  )
})
