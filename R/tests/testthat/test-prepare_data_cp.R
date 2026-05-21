testthat::test_that("prepare_data_cp joins five data frames and returns expected columns", {
  # Shared sample IDs across all PAP tables
  sids <- c("s1", "s2", "s3")

  diversity_df <- data.frame(
    sample_id = sids,
    N0 = c(3, 2, 4),
    stringsAsFactors = FALSE
  )
  mrt_df <- data.frame(
    sample_id = sids,
    group = c(1L, 1L, 2L),
    stringsAsFactors = FALSE
  )
  dcca_df <- data.frame(
    sample_id = sids,
    axis_1 = c(0.1, 0.2, 0.3),
    stringsAsFactors = FALSE
  )
  levels_df <- data.frame(
    sample_id = sids,
    age = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
  roc_df <- data.frame(
    Age  = c(100, 200, 300),
    ROC  = c(0.1, 0.2, 0.3),
    Peak = c(FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  data_pollen <- tibble::tibble(
    dataset_id = 1L,
    levels     = list(levels_df)
  )
  data_diversity <- tibble::tibble(
    dataset_id     = 1L,
    PAP_diversity  = list(diversity_df)
  )
  data_mrt <- tibble::tibble(
    dataset_id       = 1L,
    mvrt_partitions  = list(mrt_df),
    mvrt_cp          = list(numeric(0)),
    mvrt_groups_n    = 2L
  )
  data_roc <- tibble::tibble(
    dataset_id = 1L,
    PAP_roc    = list(roc_df)
  )
  data_dcca <- tibble::tibble(
    dataset_id   = 1L,
    dcca_scores  = list(dcca_df),
    dcca_grad_length = 1.5
  )

  result <- prepare_data_cp(
    data_pollen    = data_pollen,
    data_diversity = data_diversity,
    data_mrt       = data_mrt,
    data_roc       = data_roc,
    data_dcca      = data_dcca
  )

  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_true(
    all(c("dataset_id", "levels", "PAP_diversity", "mvrt_partitions",
          "dcca_scores") %in% names(result))
  )

  # The filtered PAP_diversity should still have the 3 shared sample_ids
  div <- dplyr::pull(result, PAP_diversity)[[1]]
  testthat::expect_true(all(sids %in% div$sample_id))
})

testthat::test_that("prepare_data_cp() keeps only intersecting sample_id values", {
  sids <- c("s1", "s2", "s3")

  diversity_df <- data.frame(sample_id = sids, N0 = c(3, 2, 4), stringsAsFactors = FALSE)
  mrt_df <- data.frame(sample_id = c("s1", "s2"), group = c(1L, 1L), stringsAsFactors = FALSE)
  dcca_df <- data.frame(sample_id = c("s1", "s2", "s4"), axis_1 = c(0.1, 0.2, 0.9), stringsAsFactors = FALSE)
  levels_df <- data.frame(sample_id = c("s1", "s2", "s3"), age = c(100, 200, 300), stringsAsFactors = FALSE)
  roc_df <- data.frame(Age = c(100, 200, 300), ROC = c(0.1, 0.2, 0.3), Peak = c(FALSE, TRUE, FALSE), stringsAsFactors = FALSE)

  data_pollen <- tibble::tibble(dataset_id = 1L, levels = list(levels_df))
  data_diversity <- tibble::tibble(dataset_id = 1L, PAP_diversity = list(diversity_df))
  data_mrt <- tibble::tibble(dataset_id = 1L, mvrt_partitions = list(mrt_df), mvrt_cp = list(numeric(0)), mvrt_groups_n = 2L)
  data_roc <- tibble::tibble(dataset_id = 1L, PAP_roc = list(roc_df))
  data_dcca <- tibble::tibble(dataset_id = 1L, dcca_scores = list(dcca_df), dcca_grad_length = 1.5)

  result <- prepare_data_cp(
    data_pollen = data_pollen,
    data_diversity = data_diversity,
    data_mrt = data_mrt,
    data_roc = data_roc,
    data_dcca = data_dcca
  )

  div <- dplyr::pull(result, PAP_diversity)[[1]]
  testthat::expect_identical(div$sample_id, c("s1", "s2"))
})

testthat::test_that("prepare_data_cp() output contains required list-columns", {
  sids <- c("x1", "x2")

  diversity_df <- data.frame(sample_id = sids, N0 = c(1, 2), stringsAsFactors = FALSE)
  mrt_df <- data.frame(sample_id = sids, group = c(1L, 2L), stringsAsFactors = FALSE)
  dcca_df <- data.frame(sample_id = sids, axis_1 = c(0.2, 0.4), stringsAsFactors = FALSE)
  levels_df <- data.frame(sample_id = sids, age = c(10, 20), stringsAsFactors = FALSE)
  roc_df <- data.frame(Age = c(10, 20), ROC = c(0.2, 0.4), Peak = c(FALSE, FALSE), stringsAsFactors = FALSE)

  data_pollen <- tibble::tibble(dataset_id = 5L, levels = list(levels_df))
  data_diversity <- tibble::tibble(dataset_id = 5L, PAP_diversity = list(diversity_df))
  data_mrt <- tibble::tibble(dataset_id = 5L, mvrt_partitions = list(mrt_df), mvrt_cp = list(numeric(0)), mvrt_groups_n = 2L)
  data_roc <- tibble::tibble(dataset_id = 5L, PAP_roc = list(roc_df))
  data_dcca <- tibble::tibble(dataset_id = 5L, dcca_scores = list(dcca_df), dcca_grad_length = 1.2)

  result <- prepare_data_cp(
    data_pollen = data_pollen,
    data_diversity = data_diversity,
    data_mrt = data_mrt,
    data_roc = data_roc,
    data_dcca = data_dcca
  )

  testthat::expect_true(all(c("levels", "PAP_diversity", "mvrt_partitions", "PAP_roc", "dcca_scores") %in% names(result)))
  testthat::expect_equal(nrow(result), 1L)
})
