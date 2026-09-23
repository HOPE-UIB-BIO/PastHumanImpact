testthat::test_that("build_human_proxy_time_lookup() maps source ages", {
  kk10 <-
    build_human_proxy_time_lookup("kk10", 7901L)

  hyde <-
    build_human_proxy_time_lookup("hyde_3_2", 75L)

  testthat::expect_identical(kk10[["age_bp"]][c(1L, 7901L)], c(8000, 100))
  testthat::expect_true(all(c(7950, 6950, 1949) %in% hyde[["age_bp"]]))
  testthat::expect_identical(nrow(hyde), 75L)
})

testthat::test_that("build_human_proxy_time_lookup() validates layers", {
  testthat::expect_error(
    build_human_proxy_time_lookup("hyde_3_2", 74L),
    regexp = "75 time layers"
  )
})
