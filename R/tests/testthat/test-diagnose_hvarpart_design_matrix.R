testthat::test_that("diagnose_hvarpart_design_matrix() reports rank and VIF", {
  data <- tibble::tibble(a = 1:10, b = c(1:5, 7:11))
  result <- diagnose_hvarpart_design_matrix(data, c("a", "b"), "selected")
  testthat::expect_named(result, c("vif", "condition", "design"))
  testthat::expect_equal(nrow(result[["vif"]]), 2L)
  testthat::expect_true(dplyr::pull(result[["design"]], full_rank))
  testthat::expect_error(
    diagnose_hvarpart_design_matrix(data, "missing", "selected"),
    "contract"
  )
})
