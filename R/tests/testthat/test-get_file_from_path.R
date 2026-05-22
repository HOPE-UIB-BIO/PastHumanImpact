testthat::test_that("get_file_from_path() reads an RDS object", {
  expected <-
    data.frame(
      x = c(1, 2),
      y = c("a", "b")
    )

  path_rds <-
    tempfile(fileext = ".rds")
  saveRDS(expected, path_rds)

  res_data <-
    get_file_from_path(path = path_rds)

  testthat::expect_s3_class(res_data, "data.frame")
  testthat::expect_identical(res_data, expected)
})

testthat::test_that("get_file_from_path() preserves object type", {
  expected <-
    list(
      vec_id = c("a", "b"),
      flag_valid = TRUE
    )

  path_rds <-
    tempfile(fileext = ".rds")
  saveRDS(expected, path_rds)

  res_obj <-
    get_file_from_path(path = path_rds)

  testthat::expect_type(res_obj, "list")
  testthat::expect_identical(res_obj, expected)
})

testthat::test_that("get_file_from_path() errors for missing files", {
  path_missing <-
    tempfile(fileext = ".rds")

  testthat::expect_error(
    get_file_from_path(
      path = path_missing
    )
  )
})

testthat::test_that("get_file_from_path() errors for invalid RDS content", {
  path_bad <-
    tempfile(fileext = ".rds")
  writeLines("this is not an rds object", con = path_bad)

  testthat::expect_error(
    get_file_from_path(path = path_bad),
    regexp = "unknown input format|error reading"
  )
})

testthat::test_that("get_file_from_path() validates path input type", {
  testthat::expect_error(
    get_file_from_path(path = 123),
    regexp = "single character"
  )
})
