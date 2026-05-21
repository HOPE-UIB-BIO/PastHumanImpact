testthat::test_that("get_meta_data() adds age limits and climatezone labels", {
  data_assembly <-
    data.frame(
      dataset_id = 1,
      handle = "site-1",
      country = "Country",
      long = 1,
      lat = 2,
      altitude = 3,
      depositionalenvironment = "lake",
      region = "Europe",
      curve_name = "curve-a",
      ecozone_koppen_5 = "Cold",
      ecozone_koppen_15 = "Cold_Without_dry_season",
      ecozone_koppen_30 = "Cold_Cold_Summer",
      data_publicity = "open",
      doi = "10.0000/example",
      levels = I(list(data.frame(age = c(100, 200, 300))))
    )

  result <-
    get_meta_data(data_assembly)

  vec_age_min <-
    result[["age_min"]]

  vec_age_max <-
    result[["age_max"]]

  vec_climatezone <-
    result[["climatezone"]]

  testthat::expect_identical(
    vec_age_min,
    100
  )

  testthat::expect_identical(
    vec_age_max,
    300
  )

  testthat::expect_identical(
    vec_climatezone,
    "Cold_Cold_Summer"
  )
})

testthat::test_that("get_meta_data() keeps selected metadata columns", {
  data_assembly <-
    data.frame(
      dataset_id = 2,
      handle = "site-2",
      country = "X",
      long = 5,
      lat = 6,
      altitude = 7,
      depositionalenvironment = "bog",
      region = "Asia",
      curve_name = "curve-b",
      ecozone_koppen_5 = "Temperate",
      ecozone_koppen_15 = "Temperate_Dry_Summer",
      ecozone_koppen_30 = "Temperate_Cold_Summer",
      data_publicity = "open",
      doi = "10.0000/test",
      levels = I(list(data.frame(age = c(400, 800))))
    )

  result <- get_meta_data(data_assembly)

  testthat::expect_true(all(c("dataset_id", "handle", "country", "region", "age_min", "age_max", "climatezone") %in% names(result)))
  testthat::expect_equal(nrow(result), 1L)
})

testthat::test_that("get_meta_data() climatezone logic follows configured precedence", {
  data_assembly <-
    data.frame(
      dataset_id = c(1, 2, 3),
      handle = c("a", "b", "c"),
      country = c("A", "B", "C"),
      long = c(1, 2, 3),
      lat = c(1, 2, 3),
      altitude = c(1, 2, 3),
      depositionalenvironment = c("lake", "bog", "lake"),
      region = c("Europe", "Europe", "Asia"),
      curve_name = c("c1", "c2", "c3"),
      ecozone_koppen_5 = c("Cold", "Temperate", "Arid"),
      ecozone_koppen_15 = c("Cold_Dry_Winter", "Temperate_Dry_Summer", "Arid"),
      ecozone_koppen_30 = c("Cold_Without_dry_season", "Temperate_Without_dry_season", "Arid"),
      data_publicity = c("open", "open", "open"),
      doi = c("d1", "d2", "d3"),
      levels = I(list(
        data.frame(age = c(100, 200)),
        data.frame(age = c(300, 400)),
        data.frame(age = c(500, 600))
      ))
    )

  result <- get_meta_data(data_assembly)
  testthat::expect_identical(
    dplyr::pull(result, climatezone),
    c("Cold_Dry_Winter", "Temperate_Dry_Summer", "Arid")
  )
})