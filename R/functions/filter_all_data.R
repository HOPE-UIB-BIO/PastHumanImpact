#' @title Filter out levels and sequences based on the criteria
#'
#' @param data_source Data.frame with  `dataset_id`.
#' @param variable_vec Vector with name sof columns which should be filtered.
#' By default the columns are `levels`, `raw_counts`, `counts_harmonised`, and
#' `age_uncertainty`
#' @param min_n_grains Number of individual pollen grains which each level has
#' to have
#' @param target_n_grains Number of individual pollen grains which each levels
#' 'preferably' has to have
#' @param percentage_samples Threshold of number of levels with 'preferable' counts
#' @param maximum_age_extrapolation Maximum age, which be can be extrapolated
#' beyond the oldest chronology control point
#' @param min_n_levels Minimal number of levels each sequence has to have
filter_all_data <- function(data_source,
                            variable_vec = c(
                              "levels",
                              "raw_counts",
                              "counts_harmonised",
                              "age_uncertainty"
                            ),
                            min_n_grains = 25,
                            target_n_grains = 150,
                            percentage_samples = 50,
                            maximum_age_extrapolation = 3000,
                            min_n_levels = 5) {
  #----------------------------------------------------------#
  # 1. Argument check -----
  #----------------------------------------------------------#

  RUtilpol::check_class("data_source", "data.frame")

  RUtilpol::check_class("variable_vec", "character")

  RUtilpol::check_col_names(
    "data_source",
    c(
      "dataset_id",
      variable_vec,
      "young_age",
      "old_age",
      "chron_control_limits",
      "end_of_interest_period"
    )
  )

  RUtilpol::check_class("min_n_grains", "numeric")

  RUtilpol::check_class("target_n_grains", "numeric")

  RUtilpol::check_class("percentage_samples", "numeric")

  RUtilpol::check_class("maximum_age_extrapolation", "numeric")

  RUtilpol::check_class("min_n_levels", "numeric")

  current_frame <- sys.nframe()
  current_env <- sys.frame(which = current_frame)

  #----------------------------------------------------------#
  # 2. Helper functions -----
  #----------------------------------------------------------#

  #' @title Subset data assemblage by valid `sample_id`
  #' @param data_source Data assembly. Table containing variables defined
  #' by `variable_vec`and `valid_id`.
  #' @param variable_vec Vector with names of columns which should be filtered
  #' @return Data assembly with all level-related columns filtered by `valid_id`
  subset_all_data_by_id <- function(data_source,
                                    variable_vec = c(
                                      "levels",
                                      "raw_counts",
                                      "counts_harmonised",
                                      "age_uncertainty"
                                    )) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names(
      "data_source",
      c(
        variable_vec,
        "valid_id"
      )
    )

    #' @title Subset levels in data.frame by the vector of `sample_ids`
    #' @param data_source Data.frame including `sample_id`
    #' @param level_vector Vector of `sample_ids`
    #' @return Data.frame only including the levels in `level_vector`
    subset_levels <- function(data_source,
                              level_vector) {
      RUtilpol::check_class("data_source", "data.frame")

      RUtilpol::check_col_names("data_source", "sample_id")

      RUtilpol::check_class("data_source", "data.frame")

      assertthat::assert_that(
        is.character(data_source$sample_id),
        msg = "'sample_id' in 'data_source' must be 'character'"
      )

      RUtilpol::check_class("level_vector", c("character", "logical"))

      if (
        length(level_vector) > nrow(data_source)
      ) {
        message(
          "WARNING: 'data_source' have smaller number of samples than 'level_vector'"
        )
      }

      data_work <-
        data_source %>%
        dplyr::filter(sample_id %in% level_vector)

      data_res <-
        data_work[order(match(data_work$sample_id, level_vector)), ]

      return(data_res)
    }

    #' @title Subset levels in data.frame by the vector of `sample_ids`
    #' @param data_source Matrix including `sample_id` as columns
    #' @param level_vector Vector of `sample_ids`
    #' @return Matrix only including the columns in `level_vector`
    subset_uncertainty_matrix <- function(data_source,
                                          level_vector) {
      current_frame <- sys.nframe()

      current_env <- sys.frame(which = current_frame)

      RUtilpol::check_class("data_source", "matrix")

      RUtilpol::check_class("level_vector", c("character", "logical"))

      assertthat::assert_that(
        length(level_vector) <= ncol(data_source) | all(is.na(level_vector)),
        msg = "'data_source' cannot have smaller number of samples than 'level_vector'"
      )

      data_work <-
        data_source[, colnames(data_source) %in% level_vector, drop = FALSE]

      data_res <-
        data_work[, order(match(colnames(data_work), level_vector)), drop = FALSE]

      return(data_res)
    }

    res <- data_source

    for (i in seq_along(variable_vec)) {
      sel_var <- variable_vec[i]

      if (
        sel_var == "age_uncertainty"
      ) {
        RUtilpol::check_col_names("res", "age_uncertainty")

        res <-
          res %>%
          dplyr::mutate(
            age_uncertainty = purrr::map2(
              .x = age_uncertainty,
              .y = valid_id,
              .f = ~ subset_uncertainty_matrix(
                data_source = .x,
                level_vector = .y
              )
            )
          )
      } else {
        RUtilpol::check_col_names("res", eval(sel_var))

        res <-
          res %>%
          dplyr::mutate(
            !!sel_var := purrr::map2(
              .x = get(sel_var),
              .y = valid_id,
              .f = ~ subset_levels(
                data_source = .x,
                level_vector = .y
              )
            )
          )
      }
    }

    res %>%
      dplyr::select(-valid_id) %>%
      return()
  }

  #' @title Select levels by the total pollen sum
  #' @param data_source Data.frame with pollen data
  #' @param min_n_grains A number of pollen grains
  #' @return Vector with valid sample_ids
  #' @description Calculates pollen sum and test if each level has more pollen
  #' sum than `min_n_grains`.
  get_sampleid_rowsums <- function(data_source,
             min_n_grains) {
      current_frame <- sys.nframe()

      current_env <- sys.frame(which = current_frame)

      RUtilpol::check_class("data_source", "data.frame")

      RUtilpol::check_col_names("data_source", "sample_id")

      RUtilpol::check_class("min_n_grains", "numeric")

      data_strip <-
        data_source %>%
        dplyr::select(-tidyselect::contains("sample_id"))

      data_subset <-
        data_source %>%
        dplyr::mutate(rowsum = rowSums(data_strip)) %>%
        dplyr::filter(rowsum > min_n_grains)

      if (
        nrow(data_subset) > 0
      ) {
        data_res <- data_subset

        data_res$sample_id %>%
          unique() %>%
          return()
      } else {
        return(NA_character_)
      }
    }

  #' @title Test if enough levels fulfil criteria of pollen sum
  #' @param data_counts Data.frame with pollen data
  #' @param data_levels Data.frame with level ages
  #' @param age_limit_young young age limit for selected site
  #' @param age_limit_old old age limit for selected site
  #' @param target_n_grains preferred number of pollen sum
  #' @param percentage_samples percentages of levels, which are in the age limit
  #' to be tested if their pollen sum fulfil criteria
  #' @return TRUE/FALSE (logical)
  #' @description Calculate pollen sum of each level and test if it is above
  #' target_n_grains. Compare sequence has at least X% (percentage_samples) of
  #' levels, within the age period, with enough pollen sum. Level is counted
  #' "in" period between age_limit_young and age_limit_old.
  detect_rowsum_distribution <- function(data_counts,
                                         data_levels,
                                         age_limit_young,
                                         age_limit_old,
                                         target_n_grains,
                                         percentage_samples) {
    RUtilpol::check_class("data_counts", "data.frame")

    RUtilpol::check_col_names("data_counts", "sample_id")

    RUtilpol::check_class("data_levels", "data.frame")

    RUtilpol::check_col_names("data_levels", "sample_id")

    RUtilpol::check_class("age_limit_young", "numeric")

    RUtilpol::check_class("age_limit_old", "numeric")

    RUtilpol::check_class("target_n_grains", "numeric")

    RUtilpol::check_class("percentage_samples", "numeric")

    data_strip <-
      data_counts %>%
      dplyr::select(-tidyselect::contains("sample_id"))

    data_merged <-
      data_counts %>%
      dplyr::mutate(rowsum = rowSums(data_strip)) %>%
      dplyr::select(sample_id, rowsum) %>%
      dplyr::mutate(
        above_threshold = rowsum >= target_n_grains
      ) %>%
      dplyr::inner_join(
        data_levels,
        by = ("sample_id")
      )

    RUtilpol::check_col_names("data_merged", "above_threshold")

    data_subset <-
      data_merged %>%
      dplyr::filter(age > age_limit_young) %>%
      dplyr::filter(age < age_limit_old)

    # set default
    fulfill_criteria <- FALSE

    if (
      nrow(data_subset) > 0
    ) {
      fulfill_criteria <-
        (mean(data_subset$above_threshold) * 100) > percentage_samples
    }

    return(fulfill_criteria)
  }

  #' @title Test if sequence is long enough for selected age criteria
  #' @param data_source Data.frame with level ages
  #' @param age_limit_young Young age limit for selected site
  #' @param age_limit_old Old age limit for selected site
  #' @return Logical
  #' @description Test if the youngest limit of any level is younger than
  #' age limit as well as if the oldest limit of any level is older than age
  #' limit
  detect_age_limits <- function(data_source,
                                age_limit_young,
                                age_limit_old) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "age")

    RUtilpol::check_class("age_limit_young", "numeric")

    RUtilpol::check_class("age_limit_old", "numeric")

    fulfill_criteria <- FALSE

    fulfill_criteria <-
      min(data_source$age) < age_limit_young &
        max(data_source$age) > age_limit_old

    return(fulfill_criteria)
  }

  #' @title Test if levels are within the limit of age extrapolation
  #' @param data_level Data.frame with ages of levels
  #' @param data_chron_control_limits Vector of two (min and max age of control
  #' points)
  #' @param maximum_age_extrapolation number of the maximum allowed
  #' extrapolation
  #' @return Vector with valid `sample_ids`
  #' @description Select only levels, which has the age
  #'   younger than last chron.control point + `maximum_age_extrapolation` &
  #'   older than first chron.control point - `maximum_age_extrapolation`
  get_sampleid_extrapol <- function(data_level,
                                    data_chron_control_limits,
                                    maximum_age_extrapolation) {
    RUtilpol::check_class("data_level", "data.frame")

    RUtilpol::check_class("data_chron_control_limits", "numeric")

    assertthat::assert_that(
      length(data_chron_control_limits) == 2,
      msg = "'data_chron_control_limits' must be a vector of two (min, max)"
    )

    RUtilpol::check_col_names("data_level", "age")

    assertthat::assert_that(
      length(data_chron_control_limits) == 2,
      msg = "'data_chron_control_limits' has to have 2 values"
    )

    RUtilpol::check_class("maximum_age_extrapolation", "numeric")

    data_level <-
      data_level %>%
      dplyr::mutate(
        lower = age,
        upper = age
      )

    data_subset <-
      data_level %>%
      dplyr::filter(
        upper < data_chron_control_limits[2] + maximum_age_extrapolation &
          lower > data_chron_control_limits[1] - maximum_age_extrapolation
      )

    if (
      nrow(data_subset) > 0
    ) {
      return(data_subset$sample_id)
    } else {
      return(NA)
    }
  }

  #' @title Select levels within the focal time period
  #' @param data_source Data.frame with ages of levels
  #' @param age_limit Maximum age value, marking the end of interest period for
  #' selected site
  #' @param current_year_cuttof should cut off levels be beyond current year?
  #' @param current_year_cuttof_value year value of current year
  #' @return Vector with valid sample_ids
  #' @description Exclude levels which are beyond the `age_limit`
  get_sampleid_age_lim <- function(data_source,
                                   age_limit,
                                   current_year_cuttof = TRUE,
                                   current_year_cuttof_value = -76) {
    current_frame <- sys.nframe()

    current_env <- sys.frame(which = current_frame)

    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_class("age_limit", "numeric")

    RUtilpol::check_class("current_year_cuttof", "logical")

    if (
      current_year_cuttof == TRUE
    ) {
      RUtilpol::check_class("current_year_cuttof_value", "numeric")
    }

    data_work <-
      data_source %>%
      dplyr::mutate(
        exclude = age > age_limit,
        change = c(0, diff(exclude))
      )
    if (
      current_year_cuttof == TRUE
    ) {
      data_work <-
        data_work %>%
        dplyr::filter(age > current_year_cuttof_value)
    }

    data_subset <-
      data_work %>%
      dplyr::filter(exclude == FALSE) %>%
      dplyr::select(-c(exclude, change))

    if (
      nrow(data_subset) > 0
    ) {
      data_subset$sample_id %>%
        unique() %>%
        return()
    } else {
      return(NA_character_)
    }
  }


  #----------------------------------------------------------#
  # 3. Filtering -----
  #----------------------------------------------------------#

  # Potential ways to filter data:
  #   A) Filter out LEVELS by pollen sum
  #   B) Filter out SEQUENCES based on pollen sums
  #   C) Filter out SEQUENCES based on age limits
  #   D) Filter out LEVELS by the last control point
  #   E) Filter out LEVELS beyond age limit
  #   F) Filters out SEQUENCES based on N of levels

  # remove duplicated sequences
  data_unique <-
    data_source %>%
    dplyr::distinct(dataset_id, .keep_all = TRUE)

  # - Sort LEVELS by Age -----

  RUtilpol::output_heading(
    msg = "Sorting levels by age",
    size = "h2"
  )

  data_ages_sorted <-
    data_unique %>%
    # sort the samples by ages
    dplyr::mutate(
      valid_id = purrr::map(
        .x = levels,
        .f = ~ .x %>%
          dplyr::arrange(age) %>%
          dplyr::select(sample_id) %>%
          purrr::pluck(1)
      )
    ) %>%
    # subset
    subset_all_data_by_id(
      data_source = .,
      variable_vec = variable_vec
    )

  RUtilpol::check_if_loaded(
    file_name = "data_ages_sorted",
    env = current_env
  )

  RUtilpol::check_class("data_ages_sorted", "data.frame")

  RUtilpol::output_comment("All levels were sorted by ages")

  # - Filter out LEVELS with duplicated age -----

  data_ages_unique_age <-
    data_ages_sorted %>%
    # check only levels which have unique age
    dplyr::mutate(
      valid_id = purrr::map(
        .x = levels,
        .f = ~ .x %>%
          dplyr::mutate(age_diff = c(1, diff(age))) %>%
          dplyr::filter(age_diff > 0) %>%
          dplyr::select(sample_id) %>%
          purrr::pluck(1)
      )
    ) %>%
    # subset
    subset_all_data_by_id(
      data_source = .,
      variable_vec = variable_vec
    )

  RUtilpol::stop_if_not(
    purrr::map_lgl(
      data_ages_unique_age$levels,
      ~ .x$age %>%
        duplicated() %>%
        any() %>%
        isFALSE()
    ) %>%
      all(),
    false_msg = "All levels DO NOT have unique age",
    true_msg = "All levels have unique age"
  )

  # - Filter out by pollen sum -----

  RUtilpol::output_heading(
    msg = "Filtering levels by pollen sums",
    size = "h2"
  )

  data_pollen_sum_filtered <-
    data_ages_unique_age %>%
    # get valid sample_id
    dplyr::mutate(
      valid_id = purrr::map2(
        .x = counts_harmonised,
        .y = pollen_percentage,
        .f = ~ get_sampleid_rowsums(
          data_source = .x,
          min_n_grains = ifelse(.y == FALSE,
            min_n_grains,
            0
          )
        )
      )
    ) %>%
    # subset
    subset_all_data_by_id(
      data_source = .,
      variable_vec = variable_vec
    )

  RUtilpol::check_if_loaded(
    file_name = "data_pollen_sum_filtered",
    env = current_env
  )

  RUtilpol::check_class("data_pollen_sum_filtered", "data.frame")

  RUtilpol::output_comment("All levels were filtered out by pollen sum")

  RUtilpol::output_heading(
    msg = "Filtering sequences by pollen sums",
    size = "h2"
  )

  RUtilpol::stop_if_not(
    any(
      purrr::map_lgl(data_pollen_sum_filtered$young_age, is.na),
      purrr::map_lgl(data_pollen_sum_filtered$old_age, is.na)
    ) %>%
      isFALSE(),
    false_msg = paste(
      "There are some missing data for 'young_age' and 'old_age',",
      "which are needed for selected filtering.",
      msg
    ),
    true_msg = paste("All data have a age criterium")
  )

  # test if each sequence fulfil the criteria
  #    and filter out sequences which validates the criteria
  data_percentage_filtered <-
    data_pollen_sum_filtered %>%
    dplyr::mutate(
      fullfil_test = ifelse(
        test = pollen_percentage == FALSE,
        yes = purrr::pmap_lgl(
          .l = list(counts_harmonised, levels, young_age, old_age),
          .f = ~ detect_rowsum_distribution(
            data_counts = ..1,
            data_levels = ..2,
            age_limit_young = ..3,
            age_limit_old = ..4,
            target_n_grains = target_n_grains,
            percentage_samples = percentage_samples
          )
        ),
        no = TRUE
      )
    ) %>%
    dplyr::filter(fullfil_test == TRUE) %>%
    dplyr::select(-fullfil_test)

  RUtilpol::check_if_loaded(
    file_name = "data_percentage_filtered",
    env = current_env
  )

  RUtilpol::check_class("data_percentage_filtered", "data.frame")

  RUtilpol::output_comment("All sequences were filtered out by pollen sum")

  # - Filter out SEQUENCES based on age limits  -----

  RUtilpol::output_heading(
    msg = "Filtering sequences by age limits",
    size = "h2"
  )

  RUtilpol::stop_if_not(
    any(
      purrr::map_lgl(data_percentage_filtered$young_age, is.na),
      purrr::map_lgl(data_percentage_filtered$old_age, is.na)
    ) %>%
      isFALSE(),
    false_msg = paste(
      "There are some missing data for 'young_age' and 'old_age',",
      "which are needed for selected filtering."
    ),
    true_msg = paste("All data have a age criterium")
  )

  data_age_filtered <-
    data_percentage_filtered %>%
    dplyr::mutate(
      fullfil_test = purrr::pmap_lgl(
        .l = list(levels, young_age, old_age),
        .f = ~ detect_age_limits(
          data_source = ..1,
          age_limit_young = ..2,
          age_limit_old = ..3
        )
      )
    ) %>%
    dplyr::filter(fullfil_test == TRUE) %>%
    dplyr::select(-fullfil_test)

  RUtilpol::check_if_loaded(
    file_name = "data_age_filtered",
    env = current_env
  )

  RUtilpol::check_class("data_age_filtered", "data.frame")

  RUtilpol::output_comment("All sequences were filtered out by age limits")

  #  - Filter out LEVELS by the last control point  -----

  RUtilpol::output_heading(
    msg = "Filtering out levels beyond last chron.control point",
    size = "h2"
  )

  data_extrapolation_filtered <-
    data_age_filtered %>%
    dplyr::mutate(
      valid_id = purrr::map2(
        .x = levels,
        .y = chron_control_limits,
        .f = ~ get_sampleid_extrapol(
          data_level = .x,
          data_chron_control_limits = .y,
          maximum_age_extrapolation = maximum_age_extrapolation
        )
      )
    ) %>%
    subset_all_data_by_id(
      data_source = .,
      variable_vec = variable_vec
    )

  RUtilpol::check_if_loaded(
    file_name = "data_extrapolation_filtered",
    env = current_env
  )

  RUtilpol::check_class("data_extrapolation_filtered", "data.frame")

  RUtilpol::output_comment("All levels beyond last chron.control point were filtered out")

  # - Filter out LEVELS beyond age limit  -----

  RUtilpol::output_heading(
    msg = "Filtering out levels beyond age limits",
    size = "h2"
  )

  RUtilpol::stop_if_not(
    any(
      purrr::map_lgl(
        data_extrapolation_filtered$end_of_interest_period,
        is.na
      )
    ) %>%
      isFALSE(),
    false_msg = paste(
      "There are some missing data for 'end_of_interest_period',",
      "which are needed for selected filtering."
    ),
    true_msg = paste("All data have a age criterium")
  )

  data_age_limit_filtered <-
    data_extrapolation_filtered %>%
    dplyr::mutate(
      valid_id = purrr::map2(
        .x = levels,
        .y = end_of_interest_period,
        .f = ~ get_sampleid_age_lim(
          data_source = .x,
          age_limit = .y
        )
      )
    ) %>%
    subset_all_data_by_id(
      data_source = .,
      variable_vec = variable_vec
    )

  RUtilpol::check_if_loaded(
    file_name = "data_age_limit_filtered",
    env = current_env
  )

  RUtilpol::check_class("data_age_limit_filtered", "data.frame")

  RUtilpol::output_comment("All levels beyond age limits were filtered out")


  # - Filters out SEQUENCES based on N of levels   -----

  RUtilpol::output_heading(
    msg = "Filtering out sequnces by number of levels",
    size = "h2"
  )

  data_n_leves_filtered <-
    data_age_limit_filtered %>%
    dplyr::mutate(
      n_sample_counts = purrr::map_dbl(levels, nrow)
    ) %>%
    dplyr::filter(
      n_sample_counts >= min_n_levels
    )

  RUtilpol::check_if_loaded(
    file_name = "data_n_leves_filtered",
    env = current_env
  )

  RUtilpol::check_class("data_n_leves_filtered", "data.frame")

  RUtilpol::check_col_names("data_n_leves_filtered", "n_sample_counts")

  RUtilpol::output_comment("All sequences were filtered out based on number of levels")


  data_filtered <-
    data_n_leves_filtered %>%
    # update the number of levels
    dplyr::mutate(
      n_sample_counts = purrr::map_dbl(
        .x = levels,
        .f = nrow
      ),
      age_min = purrr::map_dbl(
        .x = levels,
        .f = ~ .x %>%
          pluck("age") %>%
          min()
      ),
      age_max = purrr::map_dbl(
        .x = levels,
        .f = ~ .x %>%
          pluck("age") %>%
          max()
      )
    )

  RUtilpol::check_if_loaded(
    file_name = "data_filtered",
    env = current_env
  )

  RUtilpol::check_class("data_filtered", "data.frame")

  RUtilpol::output_comment(
    "All sequences and levels were filtered out based on user's preferences"
  )

  return(data_filtered)
}
