detect_events_from_indices <- function(ata_source_indices,
                                       data_source_pollen,
                                       sel_region = "Latin America") {
  # helper function
  detect_indices_combination <-
    function(data_source_taxa,
             data_source_indicies,
             sel_type,
             sel_dataset_id = NULL) {
      if (
        is.null(sel_dataset_id) == FALSE
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
          la_indices_table,
          sel_type = "STRONG",
          sel_dataset_id = .y
        )
      ),
      weak = purrr::map2_lgl(
        .x = data,
        .y = dataset_id,
        .f = ~ detect_indices_combination(
          data_source_taxa = .x,
          la_indices_table,
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
