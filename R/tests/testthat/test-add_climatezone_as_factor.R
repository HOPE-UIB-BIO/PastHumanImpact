testthat::test_that("add_climatezone_as_factor() recodes and labels", {
  data_input <-
    data.frame(
      climatezone = c(
        "Polar",
        "Cold_Without_dry_season_Cold_Summer",
        "Temperate_Dry_Summer",
        "Arid"
      ),
      value = c(1, 2, 3, 4)
    )

  res_data <-
    add_climatezone_as_factor(data_input)

  vec_climatezone <-
    dplyr::pull(res_data, climatezone)
  vec_climatezone_label <-
    dplyr::pull(res_data, climatezone_label)

  testthat::expect_s3_class(vec_climatezone, "factor")
  testthat::expect_s3_class(vec_climatezone_label, "factor")
  testthat::expect_identical(
    as.character(vec_climatezone),
    c("Polar", "Cold - Cold Summer", "Temperate - Dry Summer", "Arid")
  )
  testthat::expect_identical(
    as.character(vec_climatezone_label),
    c("POL", "CCS", "TDS", "ARD")
  )
})

testthat::test_that("add_climatezone_as_factor() preserves untouched values", {
  data_input <-
    data.frame(
      climatezone = c("Tropical", "Temperate", "Arid"),
      id = c(10L, 20L, 30L)
    )

  res_data <-
    add_climatezone_as_factor(data_input)

  testthat::expect_identical(
    as.character(dplyr::pull(res_data, climatezone)),
    c("Tropical", "Temperate", "Arid")
  )
  testthat::expect_identical(dplyr::pull(res_data, id), c(10L, 20L, 30L))
})

testthat::test_that("add_climatezone_as_factor() sets unknown labels to NA", {
  data_input <-
    data.frame(
      climatezone = c("Unknown_Zone", "Polar")
    )

  res_data <-
    add_climatezone_as_factor(data_input)

  vec_climatezone <-
    dplyr::pull(res_data, climatezone)
  vec_climatezone_label <-
    dplyr::pull(res_data, climatezone_label)

  testthat::expect_true(is.na(vec_climatezone[[1]]))
  testthat::expect_identical(as.character(vec_climatezone[[2]]), "Polar")
  testthat::expect_true(is.na(vec_climatezone_label[[1]]))
  testthat::expect_identical(as.character(vec_climatezone_label[[2]]), "POL")
})