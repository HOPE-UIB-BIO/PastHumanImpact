testthat::test_that("get_roc() returns only successful ROC outputs", {
  data_pollen <-
    data.frame(
      dataset_id = c(1, 2),
      pollen_percentage = c(FALSE, FALSE),
      end_of_interest_period = c(100, -1),
      counts_harmonised = I(list(
        data.frame(age = c(100, 200), taxon_a = c(10, 20)),
        data.frame(age = c(100, 200), taxon_a = c(5, 15))
      )),
      levels = I(list(
        data.frame(age = c(100, 200)),
        data.frame(age = c(100, 200))
      )),
      age_uncertainty = I(list(c(5, 5), c(5, 5))),
      stringsAsFactors = FALSE
    )

  estimate_fun <-
    function(data_source_community,
             data_source_age,
             age_uncertainty,
             smooth_method,
             smooth_N_points,
             smooth_N_max,
             smooth_age_range,
             Working_Units,
             bin_size,
             Number_of_shifts,
             bin_selection,
             rand,
             use_parallel,
             standardise,
             N_individuals,
             tranform_to_proportions,
             DC,
             interest_threshold,
             time_standardisation) {
      if (interest_threshold < 0) {
        stop("failed estimation")
      }

      data.frame(
        age = data_source_age$age,
        roc = c(0.1, 0.2)
      )
    }

  detect_fun <-
    function(data_source, sel_method, sd_threshold) {
      data_source$peak <- c(0, 1)
      data_source
    }

  res_data <-
    get_roc(
      data_pollen = data_pollen,
      verbose = FALSE,
      estimate_fun = estimate_fun,
      detect_fun = detect_fun,
      n_rand = 10
    )

  testthat::expect_identical(dplyr::pull(res_data, dataset_id), 1)
  testthat::expect_identical(names(res_data), c("dataset_id", "PAP_roc"))

  data_roc <-
    dplyr::pull(res_data, PAP_roc)[[1]]

  testthat::expect_true(all(c("age", "roc", "peak") %in% names(data_roc)))
})

testthat::test_that("get_roc() validates required input columns", {
  data_pollen <-
    data.frame(
      dataset_id = 1,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_roc(data_pollen = data_pollen, verbose = FALSE),
    regexp = "missing required columns"
  )
})

testthat::test_that("get_roc() validates function arguments", {
  data_pollen <-
    data.frame(
      dataset_id = 1,
      pollen_percentage = FALSE,
      end_of_interest_period = 100,
      counts_harmonised = I(list(data.frame(age = c(100, 200), t = c(1, 2)))),
      levels = I(list(data.frame(age = c(100, 200)))),
      age_uncertainty = I(list(c(5, 5))),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_roc(
      data_pollen = data_pollen,
      verbose = FALSE,
      estimate_fun = "not_a_function"
    ),
    regexp = "must be a function"
  )
})
