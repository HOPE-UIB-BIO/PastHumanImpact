# Function to create density of regression tree change points of the estimates of Hill's diversity, DCCA1, and MRT temporally

get_density_pap <- function(data_change_points_pap,
                        new_data_general) {
  
  data_cp_density_merge <-
    get_density_for_all_vars(
      data_source = data_change_points,
      age_table = new_data_general 
    )
  
  #--------------------------------------------------------#
  # 4. Save ----
  #--------------------------------------------------------#
  
  data_density <-
    data_cp_density_merge %>%
    dplyr::select(dataset_id, pap_density)
  
  # readr::write_rds(
  #   data_density,
  #   here::here(
  #     "Data/Processed/Partitions/PAP_density_2022-09-29.rds"
  #   ),
  #   compress = "gz"
  # )
  return(data_density)
}