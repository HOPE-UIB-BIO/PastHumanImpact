testthat::test_that("get_events_from_indices() detects strong and weak evidence", {
  data_source_indices <-
    data.frame(
      evidence = c("STRONG", "WEAK"),
      taxa_vector = I(list(c("TaxaA"), c("TaxaB"))),
      grain_eval_present = c(FALSE, TRUE),
      stringsAsFactors = FALSE
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 1,
      country = "Mexico",
      counts_harmonised = I(list(data.frame(
        sample_id = c("s1", "s2"),
        TaxaA = c(2, 0),
        TaxaB = c(0, 1)
      ))),
      levels = I(list(data.frame(
        sample_id = c("s1", "s2"),
        age = c(100, 200)
      ))),
      stringsAsFactors = FALSE
    )

  data_source_meta <-
    data.frame(
      dataset_id = 1,
      region = "Latin America",
      stringsAsFactors = FALSE
    )

  res_events <-
    get_events_from_indices(
      data_source_indices = data_source_indices,
      data_source_pollen = data_source_pollen,
      data_source_meta = data_source_meta,
      sel_region = "Latin America"
    )

  testthat::expect_identical(
    as.data.frame(res_events),
    data.frame(
      dataset_id = c(1, 1),
      age = c(100, 200),
      weak = c(FALSE, TRUE),
      strong = c(TRUE, FALSE)
    )
  )
})

testthat::test_that("get_events_from_indices() returns expected core columns", {
  data_source_indices <-
    data.frame(
      evidence = "STRONG",
      taxa_vector = I(list(c("TaxaA"))),
      grain_eval_present = FALSE,
      stringsAsFactors = FALSE
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 1,
      country = "Mexico",
      counts_harmonised = I(list(data.frame(sample_id = "s1", TaxaA = 2))),
      levels = I(list(data.frame(sample_id = "s1", age = 100))),
      stringsAsFactors = FALSE
    )

  data_source_meta <-
    data.frame(
      dataset_id = 1,
      region = "Latin America",
      stringsAsFactors = FALSE
    )

  res_events <-
    get_events_from_indices(
      data_source_indices = data_source_indices,
      data_source_pollen = data_source_pollen,
      data_source_meta = data_source_meta,
      sel_region = "Latin America"
    )

  testthat::expect_true(is.data.frame(res_events) || tibble::is_tibble(res_events))
  testthat::expect_true(
    all(c("dataset_id", "age", "weak", "strong") %in% names(res_events))
  )
})

testthat::test_that("get_events_from_indices() applies grain threshold for STRONG", {
  data_source_indices <-
    data.frame(
      evidence = "STRONG",
      taxa_vector = I(list(c("TaxaA"))),
      grain_eval_present = FALSE,
      stringsAsFactors = FALSE
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 1,
      country = "Mexico",
      counts_harmonised = I(list(data.frame(
        sample_id = c("s1", "s2"),
        TaxaA = c(1, 2)
      ))),
      levels = I(list(data.frame(
        sample_id = c("s1", "s2"),
        age = c(100, 200)
      ))),
      stringsAsFactors = FALSE
    )

  data_source_meta <-
    data.frame(
      dataset_id = 1,
      region = "Latin America",
      stringsAsFactors = FALSE
    )

  res_events <-
    get_events_from_indices(
      data_source_indices = data_source_indices,
      data_source_pollen = data_source_pollen,
      data_source_meta = data_source_meta,
      sel_region = "Latin America"
    )

  testthat::expect_identical(
    dplyr::pull(res_events, strong),
    c(FALSE, TRUE)
  )
})