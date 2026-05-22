testthat::test_that("run_pca() returns a vegan rda object on expected columns", {
  testthat::skip_if_not_installed("vegan")

  set.seed(1)
  n <- 8L
  data_pap <- data.frame(
    n0               = stats::runif(n, 5, 20),
    n1               = stats::runif(n, 3, 15),
    n2               = stats::runif(n, 2, 10),
    n1_minus_n2      = stats::runif(n, 0, 5),
    n2_divided_by_n1 = stats::runif(n, 0.3, 0.9),
    n1_divided_by_n0 = stats::runif(n, 0.3, 0.9),
    roc              = stats::runif(n, 0, 2),
    dcca_axis_1      = stats::runif(n, -1, 1),
    density_diversity = stats::runif(n, 0, 1)
  )

  res_pca <-
    run_pca(data_pap, scale = TRUE)

  testthat::expect_s3_class(res_pca, "rda")
  testthat::expect_equal(
    nrow(vegan::scores(res_pca, display = "species")),
    9L
  )
})

testthat::test_that("run_pca() returns rda with scale = FALSE", {
  testthat::skip_if_not_installed("vegan")

  set.seed(900723)
  n <- 10L
  data_pap <- data.frame(
    n0               = stats::runif(n, 5, 20),
    n1               = stats::runif(n, 3, 15),
    n2               = stats::runif(n, 2, 10),
    n1_minus_n2      = stats::runif(n, 0, 5),
    n2_divided_by_n1 = stats::runif(n, 0.3, 0.9),
    n1_divided_by_n0 = stats::runif(n, 0.3, 0.9),
    roc              = stats::runif(n, 0, 2),
    dcca_axis_1      = stats::runif(n, -1, 1),
    density_diversity = stats::runif(n, 0, 1)
  )

  res_pca <-
    run_pca(data_pap, scale = FALSE)

  testthat::expect_s3_class(res_pca, "rda")
  testthat::expect_equal(
    nrow(vegan::scores(res_pca, display = "species")),
    9L
  )
})

testthat::test_that("run_pca() errors when required columns are absent", {
  testthat::skip_if_not_installed("vegan")

  data_bad <- data.frame(x = 1:5, y = 1:5)

  testthat::expect_error(
    run_pca(data_bad),
    regexp = "n0|density_diversity"
  )
})

testthat::test_that("run_pca() validates scale argument", {
  testthat::expect_error(
    run_pca(data.frame(), scale = "yes"),
    regexp = "single logical"
  )
})
