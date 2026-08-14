testthat::test_that(
  "build_block_permutation_matrix() preserves every block",
  {
    set.seed(900723)
    vec_values <- 1:7
    vec_blocks <- c(rep("a", 3), rep("b", 3), "c")

    mat_result <-
      build_block_permutation_matrix(
        values = vec_values,
        blocks = vec_blocks,
        permutations = 10L
      )

    testthat::expect_equal(dim(mat_result), c(7L, 10L))
    testthat::expect_true(
      all(
        purrr::map_lgl(
          seq_len(ncol(mat_result)),
          .f = ~ setequal(mat_result[vec_blocks == "a", .x], 1:3)
        )
      )
    )
    testthat::expect_true(all(mat_result[vec_blocks == "c", ] == 7))
  }
)

testthat::test_that(
  "build_block_permutation_matrix() validates lengths",
  {
    testthat::expect_error(
      build_block_permutation_matrix(
        values = 1:3,
        blocks = c("a", "a"),
        permutations = 10L
      ),
      "contract"
    )
  }
)

testthat::test_that(
  "build_block_permutation_matrix() preserves sampling order",
  {
    vec_values <- 1:6
    vec_blocks <- rep(c("b", "a"), each = 3)
    set.seed(900723)
    mat_result <-
      build_block_permutation_matrix(
        values = vec_values,
        blocks = vec_blocks,
        permutations = 2L
      )
    set.seed(900723)
    vec_expected_first <-
      c(
        sample(vec_values[vec_blocks == "b"]),
        sample(vec_values[vec_blocks == "a"])
      )
    vec_expected_second <-
      c(
        sample(vec_values[vec_blocks == "b"]),
        sample(vec_values[vec_blocks == "a"])
      )

    testthat::expect_equal(mat_result[, 1], vec_expected_first)
    testthat::expect_equal(mat_result[, 2], vec_expected_second)
  }
)
