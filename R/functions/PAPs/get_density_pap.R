
#' @title Calculate density of change points
#' @description A function to get the temporal density of regression tree change points of pollen assemblage properties
#' @return A new dataset with density variables
#' 
#'
get_density_pap <- function(data_change_points_pap,
                        new_data_general) {
  
  # get_density_for_all_vars
  
  data_cp_density_merge <-
    get_density_for_all_vars(
      data_source = data_change_points,
      age_table = new_data_general 
    )

  data_density <-
    data_cp_density_merge %>%
    dplyr::select(dataset_id, pap_density)
  
  return(data_density)
}