data_provenance_sources <-
  tibble::tibble(
    source_id = c("hyde_3_5_cropland", "hyde_3_5_cell_area", "kk10"),
    product = "P",
    version = "1",
    variable = c("cropland", "garea", "land_use"),
    units = "u",
    file_relative_path = c("a.nc", "b.asc", "c.nc"),
    doi = "doi:a",
    license = "CC-BY-4.0",
    file_md5 = c("aaa", NA, "ccc"),
    notes = c("note a", NA, NA)
  )

data_provenance_checksums <-
  tibble::tibble(
    source_id = c("kk10", "hyde_3_5_cropland", "hyde_3_5_cell_area"),
    file_size_bytes_observed = c(3, 1, 2),
    file_md5_observed = c("xxx", "aaa", "bbb")
  )

data_provenance_layers <-
  tibble::tibble(
    source_id = c(
      "hyde_3_5_cropland",
      "hyde_3_5_cropland",
      "hyde_3_5_cropland",
      "hyde_3_5_cropland",
      "kk10"
    ),
    age_bp = c(450, 1950, 11950, 1850, 500)
  )

testthat::test_that("build_human_proxy_source_provenance() joins sources", {
  res <-
    build_human_proxy_source_provenance(
      data_sources = data_provenance_sources,
      data_checksums = data_provenance_checksums,
      data_layers = data_provenance_layers
    )

  testthat::expect_equal(base::nrow(res), 3L)
  testthat::expect_identical(
    dplyr::pull(res, "source_id"),
    dplyr::pull(data_provenance_sources, "source_id")
  )
  testthat::expect_true(
    base::all(
      c(
        "file_size_bytes_observed",
        "file_md5_observed",
        "md5_matches",
        "n_layers_used",
        "source_ages_bp",
        "source_ages_before_1_ce_bp",
        "notes"
      ) %in% base::names(res)
    )
  )
  testthat::expect_identical(
    dplyr::pull(res, "md5_matches"),
    c(TRUE, NA, FALSE)
  )
  testthat::expect_equal(dplyr::pull(res, "n_layers_used"), c(4L, 0L, 1L))
  testthat::expect_identical(
    dplyr::pull(res, "source_ages_bp")[[1]],
    "11950;1950;1850;450"
  )
  testthat::expect_identical(
    dplyr::pull(res, "source_ages_before_1_ce_bp")[[1]],
    "11950;1950"
  )
  testthat::expect_true(is.na(dplyr::pull(res, "source_ages_bp")[[2]]))
  testthat::expect_true(
    is.na(dplyr::pull(res, "source_ages_before_1_ce_bp")[[3]])
  )
})

testthat::test_that("build_human_proxy_source_provenance() validates", {
  testthat::expect_error(
    build_human_proxy_source_provenance(
      data_sources = data_provenance_sources,
      data_checksums = dplyr::slice(data_provenance_checksums, 1L),
      data_layers = data_provenance_layers
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    build_human_proxy_source_provenance(
      data_sources = data_provenance_sources,
      data_checksums = data_provenance_checksums,
      data_layers = tibble::tibble(source_id = "zzz", age_bp = 1)
    ),
    regexp = "contract"
  )
})
