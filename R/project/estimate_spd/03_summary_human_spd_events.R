#--------------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                
#                 Summary human spd data and events
#
#                   O. Mottl, V.A. Felde
#                         2023
#
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# 0.0. Setup -----
#--------------------------------------------------------------#

library(here)
library(furrr)

# Load configuration
source(
  here::here(
    "R/project/00_Config_file.R"
  )
)

# - Load meta data
source(
  here::here(
    "R/project/02_meta_data.R"
  )
)

#--------------------------------------------------------------#
# 1.0. Load spd 250 and 500 distance & events -----
#--------------------------------------------------------------#
# data spd 250 distance
data_spd_250_to_fit <- 
  targets::tar_read(
    name = "data_spd_to_fit",
    store = paste0(
      data_storage_path,
      "_targets_data/pipeline_predictors"
    )
  )


#--------------------------------------------------------------#
# 2.0. Summary -----
#--------------------------------------------------------------#

# summarise data with spd and no spd
data_summary_spd <-
  data_spd_250_to_fit %>% 
  pluck("data_to_fit") %>% 
  pluck(1) %>%
  left_join(data_meta %>% 
              dplyr::select(dataset_id, region, climatezone, data_publicity),
            by = "dataset_id") %>%
  filter(!(region == "Latin America" & data_publicity == "private"),
         !region == "Africa") %>%
  nest(spd = c(age, value)) %>%
  mutate(have_spd_250 = purrr::map_lgl(spd, 
                                       .f = ~ any(.x$value > 0)
  )
  ) %>%
  mutate(no_spd = purrr::map_lgl(spd, 
                                 .f = ~ all(.x$value == 0)
  )
  )

# see table 250 distance
data_summary_spd %>%
  group_by(
    region) %>%
  summarise(
    have_spd_250 = sum(have_spd_250), 
    no_spd = sum(no_spd)
  ) %>%
  knitr::kable(., "simple") 





