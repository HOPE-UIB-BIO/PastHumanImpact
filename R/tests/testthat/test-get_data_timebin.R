testthat::test_that("get_data_timebin() unnests, joins metadata, and returns n_samples count", {
  data_source <- tibble::tibble(
    dataset_id = c(1L, 2L),
    data_merge = list(
      data.frame(
        age = c(100L, 200L),
        value = c(1, 2),
        stringsAsFactors = FALSE
      ),
      data.frame(
        age = c(100L, 300L),
        value = c(3, 4),
        stringsAsFactors = FALSE
      )
    )
  )

  data_meta <- data.frame(
    dataset_id = c(1L, 2L),
    lat        = c(50.0, 48.0),
    long       = c(10.0, 12.0),
    region     = c("Europe", "Europe"),
    stringsAsFactors = FALSE
  )

  res_timebin <-
    get_data_timebin(
      data_source = data_source,
      data_meta = data_meta
    )

  testthat::expect_true(
    all(c("region", "age", "data_merge", "n_samples") %in% names(res_timebin))
  )

  # age=100 has 2 records (one per dataset), others have 1
  n_at_100 <-
    res_timebin |>
    dplyr::filter(age == 100L) |>
    dplyr::pull(n_samples)

  testthat::expect_equal(n_at_100, 2L)
})

testthat::test_that("get_data_timebin() output is a tibble", {
  data_source <- tibble::tibble(
    dataset_id = 1L,
    data_merge = list(
      data.frame(
        age = c(100L, 200L),
        value = c(5, 6),
        stringsAsFactors = FALSE
      )
    )
  )

  data_meta <- data.frame(
    dataset_id = 1L,
    lat        = 55.0,
    long       = 15.0,
    region     = "Europe",
    stringsAsFactors = FALSE
  )

  res_timebin <-
    get_data_timebin(
      data_source = data_source,
      data_meta = data_meta
    )

  testthat::expect_s3_class(res_timebin, "tbl_df")
  testthat::expect_equal(nrow(res_timebin), 2L)
  testthat::expect_type(dplyr::pull(res_timebin, n_samples), "integer")
})

testthat::test_that("get_data_timebin() groups correctly across multiple regions", {
  data_source <- tibble::tibble(
    dataset_id = c(1L, 2L),
    data_merge = list(
      data.frame(age = c(100L), value = c(1), stringsAsFactors = FALSE),
      data.frame(age = c(100L), value = c(2), stringsAsFactors = FALSE)
    )
  )

  data_meta <- data.frame(
    dataset_id = c(1L, 2L),
    lat        = c(50.0, 35.0),
    long       = c(10.0, -3.0),
    region     = c("Europe", "Iberia"),
    stringsAsFactors = FALSE
  )

  res_timebin <-
    get_data_timebin(
      data_source = data_source,
      data_meta = data_meta
    )

  vec_regions <-
    dplyr::pull(res_timebin, region)

  testthat::expect_equal(length(unique(vec_regions)), 2L)
  testthat::expect_equal(nrow(res_timebin), 2L)
})

testthat::test_that("get_data_timebin() validates required columns", {
  data_source <-
    data.frame(
      dataset_id = 1L,
      stringsAsFactors = FALSE
    )

  data_meta <-
    data.frame(
      dataset_id = 1L,
      lat = 50,
      long = 10,
      region = "Europe",
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_data_timebin(
      data_source = data_source,
      data_meta = data_meta
    ),
    regexp = "data_merge"
  )
})
