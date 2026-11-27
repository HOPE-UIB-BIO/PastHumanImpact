data_counts <-
  data.frame(sample_id = c("a", "b"), taxon_a = c(10, 20))

data_levels <-
  data.frame(sample_id = c("a", "b"), age = c(100, 200))

data_record <-
  list(
    counts_harmonised = data_counts,
    levels = data_levels,
    age_uncertainty = matrix(c(100, 200), nrow = 1),
    pollen_percentage = FALSE,
    dataset_id = 1,
    end_of_interest_period = 100
  )

settings_estimate <-
  list(
    smooth_method = "age.w",
    smooth_N_points = 5,
    smooth_N_max = 9,
    smooth_age_range = 500,
    Working_Units = "MW",
    bin_size = 500,
    Number_of_shifts = 5,
    bin_selection = "random",
    rand = 10,
    N_individuals = 150,
    DC = "chisq",
    time_standardisation = 500
  )

settings_detect <-
  list(sel_method = "trend_non_linear", sd_threshold = 2)

estimate_random <-
  function(data_source_age, use_parallel, standardise, ...) {
    res <-
      data.frame(
        age = data_source_age[["age"]],
        roc = stats::runif(n = 2L),
        use_parallel = use_parallel,
        standardise = standardise
      )

    return(res)
  }

detect_peaks <-
  function(data_source, sel_method, sd_threshold) {
    res <-
      data.frame(data_source, peak = c(FALSE, TRUE))

    return(res)
  }

testthat::test_that("compute_roc_dataset() returns peak-detected RoC", {
  res <-
    compute_roc_dataset(
      data_record = data_record,
      settings_estimate = settings_estimate,
      settings_detect = settings_detect,
      estimate_fun = estimate_random,
      detect_fun = detect_peaks,
      verbose = FALSE
    )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_true(
    base::all(c("age", "roc", "peak") %in% base::colnames(res))
  )
  testthat::expect_equal(dplyr::pull(res, peak), c(FALSE, TRUE))
})

testthat::test_that("compute_roc_dataset() runs the estimate sequentially", {
  res <-
    compute_roc_dataset(
      data_record = data_record,
      settings_estimate = settings_estimate,
      settings_detect = settings_detect,
      estimate_fun = estimate_random,
      detect_fun = detect_peaks,
      verbose = FALSE
    )

  testthat::expect_equal(dplyr::pull(res, use_parallel), c(FALSE, FALSE))
  testthat::expect_equal(dplyr::pull(res, standardise), c(TRUE, TRUE))
})

testthat::test_that("compute_roc_dataset() skips standardising percentages", {
  data_record_percentages <-
    utils::modifyList(data_record, list(pollen_percentage = TRUE))

  res <-
    compute_roc_dataset(
      data_record = data_record_percentages,
      settings_estimate = settings_estimate,
      settings_detect = settings_detect,
      estimate_fun = estimate_random,
      detect_fun = detect_peaks,
      verbose = FALSE
    )

  testthat::expect_equal(dplyr::pull(res, standardise), c(FALSE, FALSE))
})

testthat::test_that("compute_roc_dataset() resets the seed", {
  base::set.seed(900723L)

  vec_expected <-
    stats::runif(n = 2L)

  res_first <-
    compute_roc_dataset(
      data_record = data_record,
      settings_estimate = settings_estimate,
      settings_detect = settings_detect,
      estimate_fun = estimate_random,
      detect_fun = detect_peaks,
      seed = 900723L,
      verbose = FALSE
    )

  res_second <-
    compute_roc_dataset(
      data_record = data_record,
      settings_estimate = settings_estimate,
      settings_detect = settings_detect,
      estimate_fun = estimate_random,
      detect_fun = detect_peaks,
      seed = 900723L,
      verbose = FALSE
    )

  testthat::expect_equal(dplyr::pull(res_first, roc), vec_expected)
  testthat::expect_identical(res_second, res_first)
})

testthat::test_that("compute_roc_dataset() returns NA when estimation fails", {
  estimate_failing <-
    function(...) {
      stop("failed estimation")
    }

  res <-
    compute_roc_dataset(
      data_record = data_record,
      settings_estimate = settings_estimate,
      settings_detect = settings_detect,
      estimate_fun = estimate_failing,
      detect_fun = detect_peaks,
      verbose = FALSE
    )

  testthat::expect_identical(res, NA)
})

testthat::test_that("compute_roc_dataset() validates inputs", {
  data_record_incomplete <-
    data_record[base::setdiff(base::names(data_record), "levels")]

  settings_incomplete <-
    settings_estimate[base::setdiff(base::names(settings_estimate), "rand")]

  testthat::expect_error(
    compute_roc_dataset(
      data_record = data_record_incomplete,
      settings_estimate = settings_estimate,
      settings_detect = settings_detect,
      estimate_fun = estimate_random,
      detect_fun = detect_peaks
    ),
    regexp = "data_record"
  )

  testthat::expect_error(
    compute_roc_dataset(
      data_record = data_record,
      settings_estimate = settings_incomplete,
      settings_detect = settings_detect,
      estimate_fun = estimate_random,
      detect_fun = detect_peaks
    ),
    regexp = "settings"
  )

  testthat::expect_error(
    compute_roc_dataset(
      data_record = data_record,
      settings_estimate = settings_estimate,
      settings_detect = settings_detect,
      estimate_fun = "not_a_function",
      detect_fun = detect_peaks
    ),
    regexp = "must be functions"
  )

  testthat::expect_error(
    compute_roc_dataset(
      data_record = data_record,
      settings_estimate = settings_estimate,
      settings_detect = settings_detect,
      estimate_fun = estimate_random,
      detect_fun = detect_peaks,
      seed = 1.5
    ),
    regexp = "single whole number"
  )
})

testthat::test_that("compute_roc_dataset() reports progress when verbose", {
  testthat::expect_message(
    compute_roc_dataset(
      data_record = data_record,
      settings_estimate = settings_estimate,
      settings_detect = settings_detect,
      estimate_fun = estimate_random,
      detect_fun = detect_peaks,
      verbose = TRUE
    ),
    regexp = "dataset 1"
  )

  testthat::expect_no_message(
    compute_roc_dataset(
      data_record = data_record,
      settings_estimate = settings_estimate,
      settings_detect = settings_detect,
      estimate_fun = estimate_random,
      detect_fun = detect_peaks,
      verbose = FALSE
    )
  )
})
