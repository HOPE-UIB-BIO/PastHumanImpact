testthat::test_that("get_procrustes_m2() returns lower-triangular matrix with NA upper triangle", {
  testthat::skip_if_not_installed("vegan")

  make_pca <- function(seed) {
    set.seed(seed)
    mat_data <-
      matrix(
        abs(stats::rnorm(40)),
        nrow = 8L,
        ncol = 5L
      )
    colnames(mat_data) <- paste0("sp", 1:5)
    vegan::rda(mat_data, scale = FALSE)
  }

  list_pca <- list(t1 = make_pca(1), t2 = make_pca(2), t3 = make_pca(3))

  res_mat <-
    get_procrustes_m2(list_pca)

  testthat::expect_true(is.matrix(res_mat))
  testthat::expect_equal(dim(res_mat), c(3L, 3L))
  testthat::expect_equal(rownames(res_mat), c("t1", "t2", "t3"))

  testthat::expect_true(is.na(res_mat["t1", "t2"]))
  testthat::expect_true(is.na(res_mat["t1", "t3"]))
  testthat::expect_false(is.na(res_mat["t2", "t1"]))
  testthat::expect_false(is.na(res_mat["t3", "t1"]))
})

testthat::test_that("get_procrustes_m2() m2 values are in [0, 1]", {
  testthat::skip_if_not_installed("vegan")

  make_pca <- function(seed) {
    set.seed(seed)
    mat_data <-
      matrix(
        abs(stats::rnorm(40)),
        nrow = 8L,
        ncol = 5L
      )
    colnames(mat_data) <- paste0("sp", 1:5)
    vegan::rda(mat_data, scale = FALSE)
  }

  list_pca <- list(a = make_pca(10), b = make_pca(20), c = make_pca(30))

  res_mat <-
    get_procrustes_m2(list_pca)

  vec_lower <-
    res_mat[lower.tri(res_mat)]

  testthat::expect_true(all(vec_lower >= 0 & vec_lower <= 1))
})

testthat::test_that("get_procrustes_m2() handles two-element list (2x2 matrix)", {
  testthat::skip_if_not_installed("vegan")

  make_pca <- function(seed) {
    set.seed(seed)
    mat_data <-
      matrix(
        abs(stats::rnorm(20)),
        nrow = 5L,
        ncol = 4L
      )
    colnames(mat_data) <- paste0("sp", 1:4)
    vegan::rda(mat_data, scale = FALSE)
  }

  list_pca <- list(x = make_pca(7), y = make_pca(8))

  res_mat <-
    get_procrustes_m2(list_pca)

  testthat::expect_equal(dim(res_mat), c(2L, 2L))
  testthat::expect_true(is.na(res_mat["x", "y"]))
  testthat::expect_false(is.na(res_mat["y", "x"]))
})

testthat::test_that("get_procrustes_m2() validates named non-empty list input", {
  testthat::expect_error(
    get_procrustes_m2(data_list = list()),
    regexp = "must not be empty"
  )

  testthat::expect_error(
    get_procrustes_m2(data_list = list(NULL)),
    regexp = "non-empty element names"
  )
})
