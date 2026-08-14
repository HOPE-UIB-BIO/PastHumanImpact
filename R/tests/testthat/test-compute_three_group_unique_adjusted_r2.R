testthat::test_that(
  "compute_three_group_unique_adjusted_r2() preserves signed fractions",
  {
    set.seed(5)
    human <- matrix(stats::rnorm(20), ncol = 1)
    climate <- matrix(stats::rnorm(20), ncol = 1)
    time <- matrix(seq_len(20), ncol = 1)
    response <- cbind(
      human[, 1] + stats::rnorm(20),
      climate[, 1] + stats::rnorm(20)
    )
    result <-
      compute_three_group_unique_adjusted_r2(
        response = response,
        human = human,
        climate = climate,
        structure = time,
        structure_name = "time"
      )

    testthat::expect_equal(
      result$fraction,
      c(
        "pure_human", "pure_climate", "pure_time", "shared",
        "total_explained", "unexplained"
      )
    )
  }
)
