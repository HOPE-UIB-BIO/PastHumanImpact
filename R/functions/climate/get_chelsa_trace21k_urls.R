#' @title A function to select palaeoclimatic variables from CHELSA
#' @description This function create a tibble of palaeoclimatic variables, file names, and urls
#' @return A tibble to be used with the_get chelsa_download function
get_chelsa_trace21k_urls <- function(variables = c("bio", "tasmin", "tasmax", "pr"),
                                     name = "CHELSA_TraCE21k",
                                     bio_var = c(1:19),
                                     month_var = c(1:12),
                                     time_var = c(20:-220)) {
  
  base_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/chelsa_trace/"
  
  expand_grid(model = name, time_id = time_var, variable = variables,  bio = bio_var, month = month_var) %>%
    mutate(
      month = replace(month, variable == "bio", NA),
      bio = replace(bio, variable != "bio", NA),
      histdir = case_when(variable == "bio" ~ "bio/",
                          variable == "tasmin" ~ "tasmin/",
                          variable == "tasmax" ~ "tasmax/",
                          variable == "pr"~ "pr/"),
      file = case_when(variable == "bio" ~ 
                         paste0(histdir, model, "_", variable, str_pad(bio, 2, "left", "0"), "_", time_id,"_V1.0.tif"),
                       variable != "bio" ~
                         paste0(histdir, model, "_", variable, "_", month, "_", time_id, "_V1.0.tif")),
      url = paste0(base_url, file)) %>%
    #dplyr::select(file, url) %>%
    distinct()
}