testthat::test_that("save_resumable_url_file() keeps completed files", {
  destination <- tempfile(fileext = ".bin")
  writeBin(charToRaw("complete"), destination)

  result <-
    save_resumable_url_file(
      url = "https://example.org/large.bin",
      destination = destination,
      curl_command = "command-that-is-not-used"
    )

  testthat::expect_true(file.exists(result))
  testthat::expect_identical(readBin(result, "raw", 8L), charToRaw("complete"))
})

testthat::test_that("save_resumable_url_file() validates inputs", {
  testthat::expect_error(
    save_resumable_url_file("http://example.org/a", tempfile()),
    regexp = "do not satisfy"
  )
})
