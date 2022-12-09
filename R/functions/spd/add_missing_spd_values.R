add_missing_spd_values <- function(data_source_spd,
                                   data_source_meta,
                                   data_source_dummy_time) {
  data_source_meta %>%
    dplyr::distinct(dataset_id, age_min, age_max) %>%
    dplyr::left_join(
      data_source_spd,
      by = "dataset_id"
    ) %>%
    dplyr::mutate(
      spd = purrr::pmap(
        .l = list(
          spd, # ..1
          age_min, # ..2
          age_max # ..3
        ),
        .f = ~ ifelse(
          test = isTRUE(is.null(..1)),
          yes = return(
            data_source_dummy_time %>%
              dplyr::filter(
                age >= ..2,
                age <= ..3
              ) %>%
              dplyr::mutate(
                spd = 0
              )
          ),
          no = return(..1)
        )
      )
    ) %>%
    dplyr::select(-c(age_min, age_max)) %>%
    return()
}
