testthat::test_that("prepare_portable_csv_value() serialises supported values", {
  testthat::expect_identical(
    prepare_portable_csv_value(c("a", "b")),
    "a;b"
  )
  nested <- prepare_portable_csv_value(data.frame(x = 1L))
  testthat::expect_match(nested, '"x"')
  testthat::expect_error(
    prepare_portable_csv_value(baseenv()),
    "cannot be environments"
  )
})
