testthat::test_that(
  "permute_values_within_blocks() preserves block values",
  {
    set.seed(900723)
    vec_values <- 1:6
    vec_blocks <- rep(c("a", "b"), each = 3)

    vec_result <-
      permute_values_within_blocks(
        values = vec_values,
        blocks = vec_blocks
      )

    testthat::expect_setequal(vec_result[vec_blocks == "a"], 1:3)
    testthat::expect_setequal(vec_result[vec_blocks == "b"], 4:6)
  }
)

testthat::test_that(
  "permute_values_within_blocks() validates lengths",
  {
    testthat::expect_error(
      permute_values_within_blocks(
        values = 1:3,
        blocks = c("a", "a")
      ),
      "contract"
    )
  }
)
