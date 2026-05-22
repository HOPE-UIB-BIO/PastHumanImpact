#' @title Detect events based on indicators
#' @description
#' For each site and pollen level, test event indicator taxa and classify each
#' age level as weak/strong event evidence.
#' @param data_source_indicators Data frame with columns `level_2` and `evidence`.
#' @param data_source_pollen Data frame with columns `dataset_id`, `country`,
#' `counts_harmonised`, and `levels`.
#' @param data_source_meta Data frame with columns `dataset_id` and `region`.
#' @param sel_region Character scalar region name used for filtering.
#' @param country_w_pinus Character vector of countries where `Pinus` STRONG
#' evidence is excluded.
#' @return Data frame with columns `dataset_id`, `age`, `weak`, and `strong`.
get_events_from_indicators <- function(data_source_indicators,
                                       data_source_pollen,
                                       data_source_meta,
                                       sel_region = "Latin America",
                                       country_w_pinus = c("Mexico", "Guatemala", "Honduras", "Nicaragua", "Costa Rica")) {
  assertthat::assert_that(
    is.data.frame(data_source_indicators),
    msg = "`data_source_indicators` must be a data frame."
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
    all(c("level_2", "evidence") %in% names(data_source_indicators)),
    msg = "`data_source_indicators` must contain `level_2` and `evidence`."
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
    is.character(country_w_pinus),
    msg = "`country_w_pinus` must be a character vector."
  )

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

  data_indicators_evidence_raw <-
    data_subset_taxa %>%
    dplyr::left_join(
      data_source_indicators,
      by = "level_2"
    )

  data_indicators_evidence_weak <-
    data_indicators_evidence_raw %>%
    dplyr::filter(grains > 0) %>%
    dplyr::filter(evidence == "WEAK") %>%
    dplyr::group_by(
      country, dataset_id, age
    ) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      weak = purrr::map_lgl(
        .x = data,
        .f = ~ "WEAK" %in% .x$evidence
      )
    ) %>%
    dplyr::select(-data)

  data_indicators_evidence_strong <-
    data_indicators_evidence_raw %>%
    dplyr::filter(grains > 1) %>%
    dplyr::filter(evidence == "STRONG") %>%
    # filter out pinus in selected  countries where Pinus is native
    dplyr::filter(!(level_2 == "Pinus" & country %in% country_w_pinus)) %>%
    dplyr::group_by(
      country, dataset_id, age
    ) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      strong = purrr::map_lgl(
        .x = data,
        .f = ~ "STRONG" %in% .x$evidence
      )
    ) %>%
    dplyr::select(-data)

  data_levels_indicators <-
    dplyr::full_join(
      data_indicators_evidence_weak,
      data_indicators_evidence_strong,
      by = c("country", "dataset_id", "age")
    ) %>%
    dplyr::select(
      dataset_id, age, weak, strong
    ) %>%
    dplyr::mutate(
      weak = ifelse(is.na(weak), FALSE, weak),
      strong = ifelse(is.na(strong), FALSE, strong)
    )

  return(data_levels_indicators)
}
