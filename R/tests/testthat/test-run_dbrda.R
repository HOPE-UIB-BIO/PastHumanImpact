testthat::test_that("run_dbrda returns NULL when inputs are NULL", {
  result <- run_dbrda(NULL, NULL)
  testthat::expect_null(result)

  result2 <- run_dbrda(NULL, data.frame(age = 1, x = 1))
  testthat::expect_null(result2)
})

testthat::test_that("run_dbrda returns dbrda object for valid inputs", {
  testthat::skip_if_not_installed("vegan")

  set.seed(42)
  n <- 6
  comm <- matrix(c(
    0, 1, 2, 3, 4, 5,
    1, 0, 1, 2, 3, 4,
    2, 1, 0, 1, 2, 3,
    3, 2, 1, 0, 1, 2,
    4, 3, 2, 1, 0, 1,
    5, 4, 3, 2, 1, 0
  ), nrow = n, ncol = n)

  data_pred <- data.frame(
    age  = c(100, 200, 300, 400, 500, 600),
    temp = c(10, 11, 12, 13, 14, 15),
    prec = c(500, 510, 520, 530, 540, 550)
  )

  result <- run_dbrda(comm, data_pred)

  testthat::expect_true(inherits(result, "dbrda"))
})

testthat::test_that("run_dbrda validates age column when inputs are provided", {
  comm <-
    matrix(
      c(
        0, 1, 2,
        1, 0, 1,
        2, 1, 0
      ),
      nrow = 3,
      byrow = TRUE
    )

  bad_pred <-
    data.frame(
      temp = c(10, 11, 12),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    run_dbrda(comm, bad_pred),
    regexp = "age"
  )
})
