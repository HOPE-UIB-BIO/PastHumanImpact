testthat::test_that("get_events_from_indicators() applies Pinus exclusion and thresholds", {
  data_source_indicators <-
    data.frame(
      level_2 = c("Pinus", "Maize", "Weed"),
      evidence = c("STRONG", "STRONG", "WEAK"),
      stringsAsFactors = FALSE
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 1,
      country = "Mexico",
      counts_harmonised = I(list(data.frame(
        sample_id = c("s1", "s2"),
        Pinus = c(5, 0),
        Maize = c(0, 3),
        Weed = c(1, 0)
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
    get_events_from_indicators(
      data_source_indicators = data_source_indicators,
      data_source_pollen = data_source_pollen,
      data_source_meta = data_source_meta,
      sel_region = "Latin America"
    )

  testthat::expect_identical(
    as.data.frame(res_events),
    data.frame(
      dataset_id = c(1, 1),
      age = c(100, 200),
      weak = c(TRUE, FALSE),
      strong = c(FALSE, TRUE)
    )
  )
})

testthat::test_that("get_events_from_indicators() keeps Pinus as STRONG outside exclusion countries", {
  data_source_indicators <-
    data.frame(
      level_2 = "Pinus",
      evidence = "STRONG",
      stringsAsFactors = FALSE
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 1,
      country = "Peru",
      counts_harmonised = I(list(data.frame(sample_id = "s1", Pinus = 4))),
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
    get_events_from_indicators(
      data_source_indicators = data_source_indicators,
      data_source_pollen = data_source_pollen,
      data_source_meta = data_source_meta,
      sel_region = "Latin America"
    )

  testthat::expect_identical(dplyr::pull(res_events, strong), TRUE)
  testthat::expect_identical(dplyr::pull(res_events, weak), FALSE)
})

testthat::test_that("get_events_from_indicators() returns expected core columns", {
  data_source_indicators <-
    data.frame(
      level_2 = "Weed",
      evidence = "WEAK",
      stringsAsFactors = FALSE
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 1,
      country = "Mexico",
      counts_harmonised = I(list(data.frame(sample_id = "s1", Weed = 1))),
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
    get_events_from_indicators(
      data_source_indicators = data_source_indicators,
      data_source_pollen = data_source_pollen,
      data_source_meta = data_source_meta,
      sel_region = "Latin America"
    )

  testthat::expect_true(is.data.frame(res_events) || tibble::is_tibble(res_events))
  testthat::expect_true(
    all(c("dataset_id", "age", "weak", "strong") %in% names(res_events))
  )
})

testthat::test_that("get_events_from_indicators() validates required columns", {
  testthat::expect_error(
    get_events_from_indicators(
      data_source_indicators = data.frame(level_2 = "Pinus"),
      data_source_pollen = data.frame(dataset_id = 1),
      data_source_meta = data.frame(dataset_id = 1, region = "Latin America")
    ),
    regexp = "data_source_indicators"
  )
})