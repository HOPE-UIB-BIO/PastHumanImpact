get_summary_tables <- function(
  output_spatial,
  output_temporal
) {
  
  # - get summary tables spatial ----
  # full table
  summary_table_spatial <-
    data_source %>%
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
  
  # summarise to wmean values by predictor, climatezone, and region
  summary_r2_spatial <-
    spatial_summary %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c("predictor",
            "region", 
            "climatezone")
          )
        )
      ) %>% #need to fix this to weighted mean 
    dplyr::summarise(
      .groups = "drop",
      dplyr::across(
        dplyr::all_of(
          c("ratio_unique", 
            "ratio_ind")
          ),
        list(
          mean = ~ mean(.x, na.rm = TRUE)
        )
      )
    )
  
  # reshape to long table with mean values
  mean_summary_spatial <-
    summary_r2_spatial %>%
    tidyr::pivot_longer(
      dplyr::ends_with("mean"),
      names_to = "importance_type",
      values_to = "ratio"
    ) 
  
  # - get summary tables temporal ----
  # full table
  summary_table_temporal <-
    output_temporal %>%
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
  
  
  # summarise to wmean values by predictor, climatezone, and region
  summary_r2_temporal <-
    summary_table_temporal %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c("age",
            "region", 
            "predictor")
          )
        )
    ) %>% #fix here to weighted mean
    dplyr::summarise(
      .groups = "drop",
      dplyr::across(
        dplyr::all_of(
          c("ratio_unique", 
            "ratio_ind")
          ),
        list(
          mean = ~ mean(.x, na.rm = TRUE)
        )
      )
    )
  
  # reshape to long table with mean values
  mean_summary_temporal <-
    summary_r2_temporal %>%
    tidyr::pivot_longer(
      dplyr::ends_with("mean"),
      names_to = "importance_type",
      values_to = "ratio"
    ) 
  
  # - returning tables ----
  results <- list(
    summary_table_spatial = summary_table_spatial,
    summary_table_temporal = summary_table_temporal,
    mean_summary_spatial = mean_summary_spatial,
    mean_summary_temporal = mean_summary_temporal
  )
  
  return(results)
  
}







