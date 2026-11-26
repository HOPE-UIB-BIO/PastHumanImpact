testthat::test_that("compute_human_proxy_file_checksums() reads files", {
  path_a <- base::tempfile(fileext = ".txt")

  path_b <- base::tempfile(fileext = ".txt")

  base::writeLines("abc", path_a)

  base::writeLines("abcdef", path_b)

  res <-
    compute_human_proxy_file_checksums(
      data_sources = tibble::tibble(
        source_id = c("b", "a"),
        file_path = c(path_b, path_a)
      )
    )

  testthat::expect_named(
    res,
    c("source_id", "file_size_bytes_observed", "file_md5_observed")
  )
  testthat::expect_identical(dplyr::pull(res, "source_id"), c("b", "a"))
  testthat::expect_equal(
    dplyr::pull(res, "file_size_bytes_observed"),
    base::as.numeric(base::file.size(c(path_b, path_a)))
  )
  testthat::expect_identical(
    dplyr::pull(res, "file_md5_observed"),
    base::unname(tools::md5sum(c(path_b, path_a)))
  )
  testthat::expect_type(dplyr::pull(res, "file_md5_observed"), "character")
})

testthat::test_that("compute_human_proxy_file_checksums() needs files", {
  testthat::expect_error(
    compute_human_proxy_file_checksums(
      data_sources = tibble::tibble(
        source_id = "a",
        file_path = base::tempfile()
      )
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    compute_human_proxy_file_checksums(
      data_sources = tibble::tibble(source_id = "a")
    ),
    regexp = "contract"
  )
})
