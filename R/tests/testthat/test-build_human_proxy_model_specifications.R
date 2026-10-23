testthat::test_that("build_human_proxy_model_specifications() is deterministic", {
  result <- build_human_proxy_model_specifications()
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_identical(
    dplyr::pull(result, model_id),
    c("joint_filtered", "spd_matched_bridge")
  )
  testthat::expect_identical(
    result[["human_candidates"]][[1]],
    c("spd_sqrt", "kk10_fraction", "hyde_sqrt")
  )
})
