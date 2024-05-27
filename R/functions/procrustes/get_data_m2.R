get_data_m2 <- function(data_source = data_for_hvar,
                        data_meta = data_meta,
                        min_samples = 5,
                        select_vars = NULL) {
  # prepare data
  data_for_pca <-
    data_source %>%
    dplyr::inner_join(
      data_meta %>%
        dplyr::select(dataset_id, region, climatezone),
      by = "dataset_id"
    ) %>%
    tidyr::unnest(data_merge) %>%
    dplyr::select(
      c(
        "region", "climatezone",
        dplyr::all_of(select_vars)
      )
    ) %>%
    tidyr::nest(
      data = -c("age", "climatezone", "region")
    ) %>%
    dplyr::mutate(
      n_samples = purrr::map_dbl(
        .x = data,
        .f = ~ nrow(.x)
      )
    ) %>%
    dplyr::filter(n_samples >= min_samples)

  # Run PCA analyses; get procrustes sum of square matrices; extract difference with time
  pap_procrustes <-
    data_for_pca %>%
    dplyr::mutate(
      pca_analysis = purrr::map(
        .x = data,
        .f = run_pca
      )
    ) %>%
    dplyr::mutate(
      pca_analysis = pca_analysis %>%
        rlang::set_names(nm = data_for_pca$age)
    ) %>%
    dplyr::group_by(region, climatezone) %>%
    dplyr::summarise(
      pca_analysis = list(pca_analysis)
    ) %>%
    dplyr::mutate(
      m2 = purrr::map(
        .x = pca_analysis,
        .f = get_procrustes_m2
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      m2_time = purrr::map(
        .x = m2,
        .f = extract_m2_time
      )
    ) %>%
    dplyr::mutate(
      PCoA = purrr::map(
        .x = m2, .f = run_pcoa
      )
    ) %>%
    dplyr::mutate(
      m2_time_df = purrr::map(
        .x = m2_time,
        .f = get_m2_time_df
      )
    )

  return(pap_procrustes)
}
