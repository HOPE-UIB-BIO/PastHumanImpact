#' @title Detect events based on indicators
#' @description For each site, test each level with the list of event indicators
#' and clasify each level as a event type (binary).
get_events_from_indicators <- function(data_source_indicators,
                                       data_source_pollen,
                                       data_source_meta,
                                       sel_region = "Latin America",
                                       country_w_pinus = c("Mexico", "Guatemala", "Honduras", "Nicaragua", "Costa Rica")) {
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
