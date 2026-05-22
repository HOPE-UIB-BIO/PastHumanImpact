#' @title Detect events based on idices
#' @description
#' For each site and pollen level, test index taxon combinations and classify
#' each age level as weak/strong event evidence.
#' @param data_source_indices Data frame with columns `evidence`, `taxa_vector`,
#' and `grain_eval_present`.
#' @param data_source_pollen Data frame with columns `dataset_id`, `country`,
#' `counts_harmonised`, and `levels`.
#' @param data_source_meta Data frame with columns `dataset_id` and `region`.
#' @param sel_region Character scalar region name used for filtering.
#' @param verbose Logical. If `TRUE` (default), progress messages are printed.
#' @return Data frame with columns `dataset_id`, `age`, `weak`, and `strong`.
get_events_from_indices <- function(data_source_indices,
                                    data_source_pollen,
                                    data_source_meta,
                                    sel_region = "Latin America",
                                    verbose = TRUE) {
  assertthat::assert_that(
    is.data.frame(data_source_indices),
    msg = "`data_source_indices` must be a data frame."
  )

  assertthat::assert_that(
    is.data.frame(data_source_pollen),
    msg = "`data_source_pollen` must be a data frame."
  )

  assertthat::assert_that(
    is.data.frame(data_source_meta),
    msg = "`data_source_meta` must be a data frame."
  )

  assertthat::assert_that(
    all(c("evidence", "taxa_vector", "grain_eval_present") %in% names(data_source_indices)),
    msg = "`data_source_indices` must contain `evidence`, `taxa_vector`, and `grain_eval_present`."
  )

  assertthat::assert_that(
    all(c("dataset_id", "country", "counts_harmonised", "levels") %in% names(data_source_pollen)),
    msg = "`data_source_pollen` must contain `dataset_id`, `country`, `counts_harmonised`, and `levels`."
  )

  assertthat::assert_that(
    all(c("dataset_id", "region") %in% names(data_source_meta)),
    msg = "`data_source_meta` must contain `dataset_id` and `region`."
  )

  assertthat::assert_that(
    is.character(sel_region) && length(sel_region) == 1,
    msg = "`sel_region` must be a single character value."
  )

  assertthat::assert_that(
    is.logical(verbose) && length(verbose) == 1,
    msg = "`verbose` must be a single logical value."
  )

  `%>%` <- magrittr::`%>%`

  # helper function
  detect_indices_combination <-
    function(data_source_taxa,
             data_source_indicies,
             sel_type,
             sel_dataset_id = NULL) {
      if (
        isTRUE(verbose) && isFALSE(is.null(sel_dataset_id))
      ) {
        message(sel_dataset_id)
      }

      taxa_present_table <-
        data_source_taxa

      data_source_indicies %>%
        dplyr::filter(evidence == sel_type) %>%
        dplyr::mutate(
          detected = purrr::map2_lgl(
            .x = taxa_vector,
            .y = grain_eval_present,
            .f = ~ {
              taxa_present <-
                taxa_present_table %>%
                dplyr::filter(
                  grains > ifelse(.y, 0, 1)
                ) %>%
                purrr::pluck("level_2")

              all(.x %in% taxa_present) %>%
                return()
            }
          )
        ) %>%
        purrr::pluck("detected") %>%
        any() %>%
        return()
    }

  data_subset <-
    data_source_pollen %>%
    dplyr::inner_join(
      data_source_meta,
      by = "dataset_id"
    ) %>%
    dplyr::filter(
      region == sel_region
    ) %>%
    dplyr::mutate(
      counts_long = purrr::map(
        .x = counts_harmonised,
        .f = ~ .x %>%
          tidyr::pivot_longer(
            cols = -sample_id,
            names_to = "level_2",
            values_to = "grains"
          ) %>%
          dplyr::filter(
            grains > 0
          ) %>%
          dplyr::distinct(sample_id, level_2, grains)
      )
    )

  data_levels <-
    data_subset %>%
    tidyr::unnest(levels) %>%
    dplyr::select(country, dataset_id, sample_id, age)

  data_subset_taxa <-
    data_subset %>%
    dplyr::select(
      dataset_id, country, counts_long
    ) %>%
    tidyr::unnest(counts_long) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(
      data_levels,
      by = c("country", "dataset_id", "sample_id")
    ) %>%
    dplyr::select(
      country, dataset_id, age, level_2, grains
    )

  data_indicies_evidence <-
    data_subset_taxa %>%
    dplyr::group_by(country, dataset_id, age) %>%
    tidyr::nest() %>%
    dplyr::group_by(dataset_id) %>%
    dplyr::mutate(
      strong = purrr::map2_lgl(
        .x = data,
        .y = dataset_id,
        .f = ~ detect_indices_combination(
          data_source_taxa = .x,
          data_source_indicies = data_source_indices,
          sel_type = "STRONG",
          sel_dataset_id = .y
        )
      ),
      weak = purrr::map2_lgl(
        .x = data,
        .y = dataset_id,
        .f = ~ detect_indices_combination(
          data_source_taxa = .x,
          data_source_indicies = data_source_indices,
          sel_type = "WEAK",
          sel_dataset_id = .y
        )
      )
    ) %>%
    dplyr::collect() %>%
    dplyr::ungroup()

  data_levels_indicies <-
    data_indicies_evidence %>%
    dplyr::select(
      dataset_id, age, weak, strong
    )

  return(data_levels_indicies)
}
