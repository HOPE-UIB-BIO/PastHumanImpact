get_summary_tables <- function(
  output_spatial,
  output_temporal,
  data_meta
) {
  
  # - get summary tables spatial ----
  # full table
  summary_table_spatial <-
    output_spatial %>%
    dplyr::left_join(
      data_meta %>%
        dplyr::select(
          dataset_id,
          region,
          climatezone
        ), by = "dataset_id"
    ) %>% # join region and climatezone
    dplyr::mutate(
      summary_table = purrr::map(
        .x = varhp,
        .f = ~ purrr::pluck(.x, "summary_table")
      )
    ) %>%
    tidyr::unnest(summary_table) %>%
    dplyr::select(-c(data_merge, varhp)) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = Unique,
        .fns = ~ replace(., .x < 0, 0.0001)
      )
    ) %>% # negative variances can be ignored
    group_by(
      region, climatezone
    ) %>% # number records in climatezones for regions
    dplyr::mutate(
      n_records = length(unique(dataset_id))
    ) %>%
    dplyr::ungroup() %>%
    janitor::clean_names()%>%
    group_by(dataset_id) %>%
    mutate(sum_importance = sum(individual),
           ratio_unique = unique/sum_importance,
           ratio_ind = individual/sum_importance) %>%
    ungroup()
  
  # summarise ratios of importance using wmean ratios by model importance (i.e sum importance of individual predictors)
  wmean_summary_spatial <-
    summary_table_spatial %>%
    dplyr::group_by(
      predictor,
      region,
      climatezone
    ) %>% 
    dplyr::summarise(
      .groups = "drop",
      dplyr::across(
        dplyr::all_of(
          c("ratio_unique", 
            "ratio_ind")
        ),
        list(
          wmean = ~ weighted.mean(
            x = .x, 
            w = sum_importance, 
            na.rm = TRUE)
        )
      )
    ) %>%
    tidyr::pivot_longer(
      dplyr::ends_with("wmean"),
      names_to = "importance_type",
      values_to = "ratio"
    ) 
  
  # - get summary tables temporal ----
  # full table
  summary_table_temporal <-
    output_temporal  %>%
    dplyr::mutate(
      summary_table = purrr::map(
        .x = varhp,
        .f = ~ .x %>%
          purrr::pluck("summary_table")
      )
    ) %>%
    tidyr::unnest(summary_table) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = Unique,
        .fns = ~ replace(., .x < 0, 0.0001)
      )
    ) %>%
    dplyr::select(-c(data_merge, varhp)) %>%
    dplyr::ungroup() %>%
    janitor::clean_names() %>%
    group_by(age) %>%
    mutate(sum_importance = sum(individual),
           ratio_unique = unique/sum_importance,
           ratio_ind = individual/sum_importance) %>%
    ungroup()
  
  
  # summarise to wmean values by age, region, and predictor
  wmean_summary_temporal <-
    summary_table_temporal %>%
    dplyr::group_by(
      age,
      region,
      predictor
    ) %>% 
    #summarise weighted mean
    dplyr::summarise(
      .groups = "drop",
      dplyr::across(
        dplyr::all_of(
          c("ratio_unique", 
            "ratio_ind")
          ),
        list(
          wmean = ~ weighted.mean(
            x = .x, 
            w = sum_importance, 
            na.rm = TRUE)
        )
      )
    ) %>%
    tidyr::pivot_longer(
      dplyr::ends_with("wmean"),
      names_to = "importance_type",
      values_to = "ratio"
    ) 
  
  # - returning tables ----
  results <- list(
    summary_table_spatial = summary_table_spatial,
    summary_table_temporal = summary_table_temporal,
    wmean_summary_spatial = wmean_summary_spatial,
    wmean_summary_temporal = wmean_summary_temporal
  )
  
  return(results)
  
}







