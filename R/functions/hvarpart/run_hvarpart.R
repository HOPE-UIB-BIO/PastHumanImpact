#' @title Run hierarchial variation partitioning
#' @description This wrapper run the hierarchical variation partitioning for the HOPE dataset
#' @param datasource input tibble 
#' @param perm integer; number of permutations for p-values
#' @param reponse_var vector of variables to be included in the response dataset
#' @param predictor_vars vector of variables to be included in the predictor dataset
#' @param choose_matrix logical; if predictor variables should be assessed individual or as list of data frames
#' @return List of model outputs and a summary table of the results 


run_hvarpart <- function(datasource,
                   perm = 99,
                   response_var = NULL,
                   predictor_vars = NULL,
                   choose_matrix = FALSE,
                   time_series = TRUE) {
  
  res <- datasource %>% 
    mutate(varhp = purrr::map(data_merge, 
                            ~get_varhp(., 
                                       permutations = perm,   
                                       resp_vars = response_vars, 
                                       pred_vars = predictor_vars,
                                       run_matrix = choose_matrix,
                                       series = time_series))) 
  
  return(res)
  
  }