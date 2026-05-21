testthat::test_that("add_region_as_factor() recodes Latin America", {
  data_input <-
    data.frame(
      region = c("North America", "Latin America", "Europe"),
      value = c(1, 2, 3)
    )

  res_data <-
    add_region_as_factor(data_input)

  vec_region <-
    dplyr::pull(res_data, region)

  testthat::expect_s3_class(vec_region, "factor")
  testthat::expect_identical(
    as.character(vec_region),
    c("North America", "Central & South America", "Europe")
  )
})

testthat::test_that("add_region_as_factor() enforces canonical level order", {
  data_input <-
    data.frame(
      region = c("Asia", "Oceania", "Europe")
    )

  res_data <-
    add_region_as_factor(data_input)

  vec_region <-
    dplyr::pull(res_data, region)

  testthat::expect_identical(
    levels(vec_region),
    c(
      "North America",
      "Central & South America",
      "Europe",
      "Asia",
      "Oceania"
    )
  )
})

testthat::test_that("add_region_as_factor() preserves other columns", {
  data_input <-
    data.frame(
      region = c("Europe", "Asia"),
      metric = c(3.1, 4.2),
      id = c("a", "b")
    )

  res_data <-
    add_region_as_factor(data_input)

  testthat::expect_identical(dplyr::pull(res_data, metric), c(3.1, 4.2))
  testthat::expect_identical(dplyr::pull(res_data, id), c("a", "b"))
})