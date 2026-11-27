#' @title Compute the rate of change for one dataset
#' @description
#' Estimate the rate of change (RoC) of one pollen dataset and detect its peak
#' points, with the random-number generator reset to a fixed seed first.
#' @param data_record
#' Named list for one dataset with elements `counts_harmonised` (data frame),
#' `levels` (data frame), `age_uncertainty`, `pollen_percentage` (logical),
#' `dataset_id`, and `end_of_interest_period`.
#' @param settings_estimate
#' Named list of `estimate_fun` arguments shared by all datasets:
#' `smooth_method`, `smooth_N_points`, `smooth_N_max`, `smooth_age_range`,
#' `Working_Units`, `bin_size`, `Number_of_shifts`, `bin_selection`, `rand`,
#' `N_individuals`, `DC`, and `time_standardisation`.
#' @param settings_detect
#' Named list of `detect_fun` arguments: `sel_method` and `sd_threshold`.
#' @param estimate_fun
#' Function used to estimate RoC, normally `RRatepol::fc_estimate_RoC`.
#' @param detect_fun
#' Function used to detect peak points, normally
#' `RRatepol::fc_detect_peak_points`.
#' @param seed
#' Whole number to which the random-number generator is reset before the
#' estimation.
#' @param verbose
#' Logical. If `TRUE` (default), a progress message names the dataset.
#' @return
#' The data frame returned by `detect_fun`, or `NA` when the estimation or
#' the peak detection fails.
#' @details
#' `estimate_fun` is called with `use_parallel = FALSE`, so all random draws
#' (age-uncertainty draws, level selection within bins, and subsampling of
#' individuals) happen in the current process after the seed is reset. The
#' result therefore depends only on the dataset, the settings, and the seed.
#' Counts are standardised and transformed to proportions only when
#' `pollen_percentage` is `FALSE`. The function changes the global
#' random-number state.
#' @examples
#' \dontrun{
#' compute_roc_dataset(
#'   data_record = list_records[[1]],
#'   settings_estimate = settings_estimate,
#'   settings_detect = list(sel_method = "trend_non_linear", sd_threshold = 2),
#'   estimate_fun = RRatepol::fc_estimate_RoC,
#'   detect_fun = RRatepol::fc_detect_peak_points
#' )
#' }
compute_roc_dataset <- function(
    data_record,
    settings_estimate,
    settings_detect,
    estimate_fun,
    detect_fun,
    seed = 1234L,
    verbose = TRUE
) {
  vec_record_names <-
    c(
      "counts_harmonised",
      "levels",
      "age_uncertainty",
      "pollen_percentage",
      "dataset_id",
      "end_of_interest_period"
    )

  vec_estimate_names <-
    c(
      "smooth_method",
      "smooth_N_points",
      "smooth_N_max",
      "smooth_age_range",
      "Working_Units",
      "bin_size",
      "Number_of_shifts",
      "bin_selection",
      "rand",
      "N_individuals",
      "DC",
      "time_standardisation"
    )

  assertthat::assert_that(
    is.list(data_record),
    all(vec_record_names %in% names(data_record)),
    is.logical(data_record[["pollen_percentage"]]),
    length(data_record[["pollen_percentage"]]) == 1L,
    msg = paste(
      "`data_record` must be a list with the dataset tables and a logical",
      "`pollen_percentage`."
    )
  )

  assertthat::assert_that(
    is.list(settings_estimate),
    all(vec_estimate_names %in% names(settings_estimate)),
    is.list(settings_detect),
    all(c("sel_method", "sd_threshold") %in% names(settings_detect)),
    msg = "`settings_estimate` or `settings_detect` is missing arguments."
  )

  assertthat::assert_that(
    is.function(estimate_fun),
    is.function(detect_fun),
    msg = "`estimate_fun` and `detect_fun` must be functions."
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
    is.logical(verbose),
    length(verbose) == 1L,
    !is.na(verbose),
    msg = "`verbose` must be TRUE or FALSE."
  )

  if (
    isTRUE(verbose)
  ) {
    value_dataset_id <-
      data_record[["dataset_id"]]

    cli::cli_inform("Estimating RoC for dataset {value_dataset_id}.")
  }

  is_counts <-
    !data_record[["pollen_percentage"]]

  set.seed(seed)

  res <-
    tryCatch(
      expr = {
        data_estimate <-
          estimate_fun(
            data_source_community = data_record[["counts_harmonised"]],
            data_source_age = data_record[["levels"]],
            age_uncertainty = data_record[["age_uncertainty"]],
            smooth_method = settings_estimate[["smooth_method"]],
            smooth_N_points = settings_estimate[["smooth_N_points"]],
            smooth_N_max = settings_estimate[["smooth_N_max"]],
            smooth_age_range = settings_estimate[["smooth_age_range"]],
            Working_Units = settings_estimate[["Working_Units"]],
            bin_size = settings_estimate[["bin_size"]],
            Number_of_shifts = settings_estimate[["Number_of_shifts"]],
            bin_selection = settings_estimate[["bin_selection"]],
            rand = settings_estimate[["rand"]],
            use_parallel = FALSE,
            standardise = is_counts,
            N_individuals = settings_estimate[["N_individuals"]],
            tranform_to_proportions = is_counts,
            DC = settings_estimate[["DC"]],
            interest_threshold = data_record[["end_of_interest_period"]],
            time_standardisation = settings_estimate[["time_standardisation"]]
          )

        detect_fun(
          data_source = data_estimate,
          sel_method = settings_detect[["sel_method"]],
          sd_threshold = settings_detect[["sd_threshold"]]
        )
      },
      error = function(error_condition) {
        NA
      }
    )

  return(res)
}
