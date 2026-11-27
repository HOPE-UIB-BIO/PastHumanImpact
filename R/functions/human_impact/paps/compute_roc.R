#' @title Compute Rate-of-change estimates
#' @description Use RRatepol to estimate RoC of pollen sequences
#' @param data_pollen Pollen counts but if not available can use percentages
#' @param smoothing_method
#' @param min_points_smoothing
#' @param age_range_smoothing
#' @param working_units_selection if run in parallel
#' @param size_of_bin in ages
#' @param n_mowing_windows
#' @param which_level_select_in_bin
#' @param n_rand number of randomisations
#' @param n_individuals_to_standardise set to  150 grains
#' @param transformation_coef coefficient distances
#' @param peak_point_method Peak-point detection method passed to
#'   `detect_fun`.
#' @param sd_for_peak_detection Numeric SD threshold for peak detection.
#' @param verbose Logical. If `TRUE` (default), progress messages are printed.
#' @param estimate_fun Function used to estimate rate of change. Defaults to
#'   `RRatepol::fc_estimate_RoC`.
#' @param detect_fun Function used to detect peak points. Defaults to
#'   `RRatepol::fc_detect_peak_points`.
#' @param seed Whole number to which the random-number generator is reset
#'   before each dataset.
#' @param n_cores Positive whole number of parallel worker processes. With
#'   `1` (default), datasets are processed in the current R session.
#' @return For each sequence the ROC results
#' @details
#' Each dataset is passed to `compute_roc_dataset()`, which resets the
#' random-number generator to `seed` and runs `estimate_fun` without its own
#' parallelisation. Parallel work is spread over datasets instead. The result
#' for a dataset therefore depends only on its own data, the settings, and the
#' seed, not on the order or number of datasets or on `n_cores`.
#'
compute_roc <- function(data_pollen,
                    smoothing_method = "age.w",
                    min_points_smoothing = 5,
                    max_points_smoothing = 9,
                    age_range_smoothing = 500,
                    working_units_selection = "MW",
                    size_of_bin = 500,
                    n_mowing_windows = 5,
                    which_level_select_in_bin = "random",
                    n_rand = 1000,
                    n_individuals_to_standardise = 150,
                    transformation_coef = "chisq",
                    peak_point_method = "trend_non_linear",
                    sd_for_peak_detection = 2,
                    verbose = TRUE,
                    estimate_fun = RRatepol::fc_estimate_RoC,
                    detect_fun = RRatepol::fc_detect_peak_points,
                    seed = 1234L,
                    n_cores = 1L,
                    ...) {
  assertthat::assert_that(
    is.data.frame(data_pollen),
    msg = "`data_pollen` must be a data frame."
  )
  req_columns <- c(
    "counts_harmonised",
    "levels",
    "age_uncertainty",
    "pollen_percentage",
    "dataset_id",
    "end_of_interest_period"
  )
  assertthat::assert_that(
    all(req_columns %in% names(data_pollen)),
    msg = "`data_pollen` is missing required columns."
  )
  assertthat::assert_that(
    is.logical(verbose),
    length(verbose) == 1,
    !is.na(verbose),
    msg = "`verbose` must be TRUE or FALSE."
  )
  assertthat::assert_that(
    is.function(estimate_fun),
    msg = "`estimate_fun` must be a function."
  )
  assertthat::assert_that(
    is.function(detect_fun),
    msg = "`detect_fun` must be a function."
  )
  assertthat::assert_that(
    is.numeric(seed),
    length(seed) == 1L,
    is.finite(seed),
    seed == round(seed),
    abs(seed) <= .Machine[["integer.max"]],
    msg = "`seed` must be a single whole number."
  )
  assertthat::assert_that(
    is.numeric(n_cores),
    length(n_cores) == 1L,
    is.finite(n_cores),
    n_cores == round(n_cores),
    n_cores >= 1,
    msg = "`n_cores` must be a positive whole number."
  )

  list_records <-
    purrr::pmap(
      .l = data_pollen[req_columns],
      .f = list
    )

  settings_estimate <-
    list(
      smooth_method = smoothing_method,
      smooth_N_points = min_points_smoothing,
      smooth_N_max = max_points_smoothing,
      smooth_age_range = age_range_smoothing,
      Working_Units = working_units_selection,
      bin_size = size_of_bin,
      Number_of_shifts = n_mowing_windows,
      bin_selection = which_level_select_in_bin,
      rand = n_rand,
      N_individuals = n_individuals_to_standardise,
      DC = transformation_coef,
      time_standardisation = size_of_bin
    )

  settings_detect <-
    list(
      sel_method = peak_point_method,
      sd_threshold = sd_for_peak_detection
    )

  if (
    n_cores == 1
  ) {
    list_roc <-
      purrr::map(
        .x = list_records,
        .f = ~ compute_roc_dataset(
          data_record = .x,
          settings_estimate = settings_estimate,
          settings_detect = settings_detect,
          estimate_fun = estimate_fun,
          detect_fun = detect_fun,
          seed = seed,
          verbose = verbose
        )
      )
  } else {
    if (
      isTRUE(verbose)
    ) {
      n_records <-
        length(list_records)

      cli::cli_inform(
        "Estimating RoC for {n_records} datasets on {n_cores} cores."
      )
    }

    cluster_roc <-
      parallel::makeCluster(n_cores)

    on.exit(
      parallel::stopCluster(cluster_roc),
      add = TRUE
    )

    # Workers must find the same package library as this session.
    parallel::clusterCall(
      cl = cluster_roc,
      fun = .libPaths,
      .libPaths()
    )

    list_roc <-
      parallel::parLapplyLB(
        cl = cluster_roc,
        X = list_records,
        fun = compute_roc_dataset,
        settings_estimate = settings_estimate,
        settings_detect = settings_detect,
        estimate_fun = estimate_fun,
        detect_fun = detect_fun,
        seed = seed,
        verbose = FALSE
      )
  }

  data_work_roc <-
    data_pollen %>%
    dplyr::mutate(
      PAP_roc = list_roc
    )

  data_roc <-
    data_work_roc %>%
    dplyr::filter(
      purrr::map_lgl(
        .x = data_work_roc$PAP_roc,
        .f = ~ "data.frame" %in% class(.x)
      )
    ) %>%
    dplyr::select(dataset_id, PAP_roc)

  return(data_roc)
}
