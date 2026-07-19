testthat::test_that("get_model_seed() is stable and model specific", {
  result_first <-
    get_model_seed(
      model_id = c("model_a", "model_b"),
      seed_attempt = c(1L, 1L),
      seed_base = 1234L
    )

  result_second <-
    get_model_seed(
      model_id = c("model_a", "model_b"),
      seed_attempt = c(1L, 1L),
      seed_base = 1234L
    )

  testthat::expect_identical(result_first, result_second)
  testthat::expect_length(unique(result_first), 2L)
  testthat::expect_true(all(result_first > 0L))
})

testthat::test_that("get_model_seed() changes across attempts", {
  result <-
    get_model_seed(
      model_id = rep("model_a", times = 3),
      seed_attempt = 1:3,
      seed_base = 1234L
    )

  testthat::expect_length(unique(result), 3L)
})

testthat::test_that("get_model_seed() validates inputs", {
  testthat::expect_error(
    get_model_seed(
      model_id = "model_a",
      seed_attempt = 0L
    ),
    regexp = "positive integers"
  )

  testthat::expect_error(
    get_model_seed(
      model_id = c("model_a", "model_b"),
      seed_attempt = 1:3
    ),
    regexp = "same length"
  )
})
