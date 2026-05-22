testthat::test_that("add_logical_rules() applies Europe and Oceania rules", {
  data_source <-
    data.frame(
      dataset_id = c(1, 2),
      region = c("Europe", "Oceania"),
      events_binary = I(list(
        data.frame(
          age = c(10, 20),
          cc = c(1, 0),
          ec = c(0, 0),
          fi = c(1, 1),
          fc = c(0, 1)
        ),
        data.frame(
          age = c(10, 20),
          weak = c(0, 1),
          medium = c(0, 0),
          strong = c(0, 0)
        )
      ))
    )

  res_data <-
    add_logical_rules(data_source)

  testthat::expect_identical(
    dplyr::pull(res_data, dataset_id),
    c(1, 2)
  )
  testthat::expect_identical(
    purrr::pluck(res_data, "events_updated", 1),
    data.frame(
      age = c(10, 20),
      bi = c(0, 0),
      fi = c(0, 1),
      fc = c(0, 1),
      ec = c(1, 0),
      cc = c(1, 0)
    )
  )
  testthat::expect_identical(
    purrr::pluck(res_data, "events_updated", 2),
    data.frame(
      age = c(10, 20),
      no_impact = c(1, 0),
      weak = c(0, 1),
      medium = c(0, 0),
      strong = c(0, 0)
    )
  )
})

testthat::test_that("add_logical_rules() returns North America output contract", {
  data_source <-
    data.frame(
      dataset_id = 9,
      region = "North America",
      events_binary = I(list(data.frame(
        age = c(10, 20, 30),
        fc_start = c(1, 0, 0),
        fc_end = c(0, 0, 0),
        fc_start_02 = c(0, 0, 0),
        fc_end_02 = c(0, 0, 0),
        fc_start_03 = c(0, 0, 0),
        fc_end_03 = c(0, 0, 0),
        fc_start_04 = c(0, 0, 0),
        fc_end_04 = c(0, 0, 0),
        es = c(0, 0, 0)
      )))
    )

  res_data <-
    add_logical_rules(data_source)

  data_events <-
    dplyr::pull(res_data, events_updated)[[1]]

  testthat::expect_identical(names(data_events), c("age", "bi", "fc", "es"))
  testthat::expect_equal(nrow(data_events), 3L)
})

testthat::test_that("add_logical_rules() errors for unsupported regions", {
  data_source <-
    data.frame(
      dataset_id = 1,
      region = "Atlantis",
      events_binary = I(list(data.frame(age = c(1, 2), x = c(0, 1))))
    )

  testthat::expect_error(
    add_logical_rules(data_source),
    regexp = "Unsupported region"
  )
})

testthat::test_that("add_logical_rules() validates required columns", {
  testthat::expect_error(
    add_logical_rules(
      data_source = data.frame(dataset_id = 1, region = "Europe")
    ),
    regexp = "must contain"
  )
})