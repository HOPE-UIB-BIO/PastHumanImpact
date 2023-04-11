 
get_interpolated_data <- function(data_source, 
                                      variable = "var_name",
                                      vars_interpolate = c("age", "value"),
                                      group_var = "dataset_id",
                                      method = c("constant", "linear"), 
                                      rule = 1:2,
                                      ties = "ordered",
                                      age_min = 0,
                                      age_max = 12e03,
                                      timestep = 500,
                                      verbose = TRUE) { 
    
    RUtilpol::check_class("variable", "character")
    RUtilpol::check_class("vars_interpolate", "character")
    RUtilpol::check_class("method", "character")
    RUtilpol::check_class("data_source", "data.frame")
    RUtilpol::check_class("verbose", "logical")
    
    if (isTRUE(verbose)) {
      RUtilpol::output_comment("getting linear interpolated values")
    }
    
    n_datasets <- data_source %>% 
      unnest(data_to_fit) %>%
      dplyr::distinct(get(group_var)) %>% 
      purrr::pluck(1) %>% 
      length()
    
    if (isTRUE(verbose)) {
      RUtilpol::output_comment(paste("N datasets:", n_datasets))
    }
    
    suppressWarnings(res <- 
                       data_source %>% 
                       unnest(data_to_fit) %>%
                       dplyr::select(dplyr::all_of(c(group_var, variable, vars_interpolate))) %>% 
                       dplyr::group_by(get(group_var)) %>% 
                       tidyr::nest(data = dplyr::any_of(vars_interpolate)) %>% 
                       tidyr::drop_na(data) %>% 
                       dplyr::ungroup() %>% 
                       dplyr::mutate(n_levels = 
                                       purrr::map_dbl(
                                         .x = data, 
                                         .f = nrow)) %>% 
                       dplyr::mutate(mod = 
                                       purrr::pmap(
                                         .l = list(data,
                                                   get(group_var), 
                                                   n_levels), 
                                         .f = ~ {
                                           if (isTRUE(verbose)) {
                                             message(..2)
                                             }
                                          approx(xy.coords(..1),
                                             xout = seq(
                                               age_min,
                                               age_max,
                                               by = timestep
                                             ),
                                             ties = ties,
                                             method = method,
                                             rule = rule)
                                           })))
    
    
    
    # new interpolated data
   data_interpolated <- res %>%
     mutate(int_data = 
              purrr::map(.x = mod, 
                         function(.x) {
                
          data.frame(.x) %>%
                  rename(age = x,
                         fit = y) %>%
                  as_tibble() %>%
                  return()
     })) %>% 
       tidyr::unnest(int_data) %>%
       dplyr::select(
         var_name, dataset_id, age, fit
       ) %>%
       tidyr::pivot_wider(
         names_from = var_name,
         values_from = "fit"
       ) %>%
       tidyr::nest(data = -c(dataset_id)) 
   
       return(data_interpolated)
     
  } 
  

  
 