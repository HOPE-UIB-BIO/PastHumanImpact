testthat::test_that("time control accepts three locally selected human columns", {
  set.seed(44)
  n <- 18L
  data <- tibble::tibble(
    age = seq(2000, by = 500, length.out = n),
    response_1 = stats::rnorm(n), response_2 = stats::rnorm(n),
    h1 = stats::rnorm(n), h2 = stats::rnorm(n), h3 = stats::rnorm(n),
    c1 = stats::rnorm(n), c2 = stats::rnorm(n)
  )
  result <- fit_temporal_hvarpart_dataset(
    data, c("response_1", "response_2"),
    list(human = c("h1", "h2", "h3"), climate = c("c1", "c2")),
    min_unique_ages = 10L, min_residual_df = 4L,
    distance_years = 500, permutations = 9L
  )
  testthat::expect_true(result$status %in%
    c("estimated", "estimated_residual_temporal_dependence"))
  testthat::expect_true(result$design_full_rank)
})

testthat::test_that("spatial control accepts three locally selected human columns", {
  set.seed(45)
  n <- 35L
  data <- tibble::tibble(
    dataset_id = as.character(seq_len(n)),
    long = stats::runif(n, -10, 20), lat = stats::runif(n, 35, 65),
    response_1 = stats::rnorm(n), response_2 = stats::rnorm(n),
    h1 = stats::rnorm(n), h2 = stats::rnorm(n), h3 = stats::rnorm(n),
    c1 = stats::rnorm(n), c2 = stats::rnorm(n)
  )
  result <- fit_spatial_hvarpart_group(
    data, c("response_1", "response_2"),
    list(human = c("h1", "h2", "h3"), climate = c("c1", "c2")),
    permutations = 9L, min_unique_locations = 10L,
    min_residual_df = 4L, distance_km = 500
  )
  testthat::expect_true(result$status %in%
    c("spatial_model_estimated", "no_spatial_terms_selected"))
})

testthat::test_that("final temporal rank failure remains explicit", {
  n <- 14L
  data <- tibble::tibble(
    age = seq(2000, by = 500, length.out = n),
    response_1 = stats::rnorm(n), response_2 = stats::rnorm(n),
    h1 = seq_len(n), h2 = seq_len(n), climate = stats::rnorm(n)
  )
  result <- fit_temporal_hvarpart_dataset(
    data, c("response_1", "response_2"),
    list(human = c("h1", "h2"), climate = "climate"),
    min_unique_ages = 10L, min_residual_df = 4L, permutations = 9L
  )
  testthat::expect_identical(result$status, "rank_deficient")
})
