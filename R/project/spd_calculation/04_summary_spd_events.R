#--------------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                
#                    Summary spd and events
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
# 1.0. Load data spd combine  -----
#--------------------------------------------------------------#


# - load data spd distance 250 ----
data_spd_combine <-
  RUtilpol::get_latest_file(
    file_name = "data_spd_combine",
    dir = paste0(
      data_storage_path,
      "Data/spd/"
    )
  )

#--------------------------------------------------------------#
# 2.0. Summary of spd data -----
#--------------------------------------------------------------#

# summarise data with spd and no spd ----
table_spd <-
  data_spd_combine  %>%
  left_join(data_meta %>% 
              dplyr::select(
                dataset_id, 
                region, 
                climatezone, 
                data_publicity),
            by = "dataset_id") %>%
  filter(!(region == "Latin America" & data_publicity == "private"),
         !region == "Africa") %>%
  mutate(have_spd = purrr::map_lgl(spd, 
                                       .f = ~ any(.x$value > 0)
  )
  ) %>%
  mutate(no_spd = purrr::map_lgl(spd, 
                                 .f = ~ all(.x$value == 0)
  )
  )

# summarise datasets per region with spd ----
table_spd <-
  table_spd %>%
  group_by(
    region,
    distance) %>%
  summarise(
    have_spd = sum(have_spd), 
    no_spd = sum(no_spd)
  ) %>%
  knitr::kable(., "simple") 

#--------------------------------------------------------------#
# 3.0. Summarise sites with events & spd -----
#--------------------------------------------------------------#

