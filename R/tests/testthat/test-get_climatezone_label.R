testthat::test_that("get_climatezone_label() maps known labels", {
  vec_label <-
    c(
      "Polar",
      "Cold - Warm Summer",
      "Temperate",
      "Arid"
    )

  res_label <-
    get_climatezone_label(vec_label)

  testthat::expect_identical(
    res_label,
    c("POL", "CWS", "TMP", "ARD")
  )
})

testthat::test_that("get_climatezone_label() returns NA for unknown values", {
  vec_label <-
    c(
      "Unknown",
      "Polar",
      "Not A Climate"
    )

  res_label <-
    get_climatezone_label(vec_label)

  testthat::expect_true(is.na(res_label[[1]]))
  testthat::expect_identical(res_label[[2]], "POL")
  testthat::expect_true(is.na(res_label[[3]]))
})

testthat::test_that("get_climatezone_label() keeps length and type", {
  vec_label <-
    c("Temperate", NA_character_, "Tropical")

  res_label <-
    get_climatezone_label(vec_label)

  testthat::expect_type(res_label, "character")
  testthat::expect_length(res_label, length(vec_label))
  testthat::expect_identical(res_label[[1]], "TMP")
  testthat::expect_true(is.na(res_label[[2]]))
  testthat::expect_identical(res_label[[3]], "TRO")
})

testthat::test_that("get_climatezone_label() handles empty input", {
  res_label <-
    get_climatezone_label(character(0))

  testthat::expect_type(res_label, "character")
  testthat::expect_length(res_label, 0L)
})

testthat::test_that("get_climatezone_label() validates character input", {
  testthat::expect_error(
    get_climatezone_label(1:3),
    regexp = "must be a character vector"
  )
})