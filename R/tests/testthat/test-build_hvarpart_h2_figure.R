testthat::test_that("H2 composite builder validates its inputs", {
  testthat::expect_error(
    build_hvarpart_h2_figure(
      output_h2 = tibble::tibble(),
      data_meta = tibble::tibble(),
      summary_zero_truncated = tibble::tibble(),
      summary_signed = tibble::tibble()
    ),
    "inputs or required packages"
  )
})
