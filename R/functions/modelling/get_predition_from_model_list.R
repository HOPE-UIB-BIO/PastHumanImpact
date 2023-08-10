get_predition_from_model_list <- function(data_source_list, dummy_table) {
  list_mods_pred <-
    purrr::map(
      .progress = TRUE,
      .x = data_source_list,
      .f = purrr::possibly(
        ~ predict_hgam(
          data_model = .x,
          data_dummy = dummy_table,
          time_step = diff(dummy_table$age) %>%
            unique()
        ),
        otherwise = NA_character_
      )
    ) %>%
    rlang::set_names(
      nm = names(data_source_list)
    ) %>%
    purrr::discard(
      .p = is.na(.)
    )

  data_mods_pred <-
    dplyr::bind_rows(
      .id = "full_name",
      list_mods_pred
    ) %>%
    dplyr::group_by(full_name) %>%
    tidyr::nest(
      data_pred = -full_name
    ) %>%
    dplyr::ungroup()

  return(data_mods_pred)
}
