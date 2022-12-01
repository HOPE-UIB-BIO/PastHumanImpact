

# A function to create a set of URLs for downloading CHELSA TraCE21k

get_chelsa_trace21k_urls <- function(variables = c("bio", "tasmin", "tasmax", "pr"),
                                     name = "CHELSA_TraCE21k",
                                     bio.var = c(1:19),
                                     month.var = c(1:12),
                                     time.var = c(20:-220)) {
  
  base_url <- "https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/chelsa_trace/"
  
  expand_grid(model = name, time.id = time.var, variable = variables,  bio = bio.var, month = month.var) %>%
    mutate(
      month = case_when(variable == "bio" ~ month == NA),
      histdir = case_when(variable == "bio" ~ "bio/",
                        variable == "tasmin" ~ "tasmin/",
                        variable == "tasmax" ~ "tasmax/",
                        variable == "pr"~ "pr/"),
      file = case_when(variable == "bio" ~ 
                     paste0(histdir, model, "_", variable, str_pad(bio, 2, "left", "0"), "_", time.id,"_V1.0.tif"),
                   variable != "bio" ~
                     paste0(histdir, model, "_", variable, str_pad(month, 2, "left", "0"), "_", time.id, "_V1.0.tif")),
  url = paste0(base_url, file)) %>%
    dplyr::select(file, url) %>%
    distinct()
}

