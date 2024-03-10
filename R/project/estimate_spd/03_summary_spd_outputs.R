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

# Load old output of spd calculations
local_storage_path <- "C:/Users/vivia/OneDrive - University of Bergen/HOPE_data_copy/HOPE_Hypothesis1/_targets1"

data_spd_old <-
  targets::tar_read(
    name = "data_spd_to_fit",
    store = paste0(
     local_storage_path
    )
  )

#--------------------------------------------------------------#
# 2.0. Summary of spd old data -----
#--------------------------------------------------------------#
# summarise data with spd and no spd
data_summary_spd_500 <-
  data_spd_old %>% 
  dplyr::filter(var_name == 500) %>%
  pluck("data_to_fit") %>% 
  pluck(1) %>%
  left_join(data_meta %>% 
              dplyr::select(dataset_id, region, climatezone, data_publicity),
            by = "dataset_id") %>%
  filter(!(region == "Latin America" & data_publicity == "private"),
         !region == "Africa") %>%
  nest(spd = c(age, value)) %>%
  mutate(have_spd_500 = purrr::map_lgl(spd, 
                                       .f = ~ any(.x$value > 0)
  )
  ) %>%
  mutate(no_spd = purrr::map_lgl(spd, 
                                 .f = ~ all(.x$value == 0)
  )
  )

# see table 500 distance
data_summary_spd_500 %>%
  group_by(
    region) %>%
  summarise(
    have_spd_500 = sum(have_spd_500), 
    no_spd = sum(no_spd)
  ) %>%
  knitr::kable(., "simple") 


# vector with datasets with spd 500 ----
have_spd_500 <- 
  data_summary_spd_500 %>% 
  filter(have_spd_500 == TRUE) %>%
  pluck("dataset_id")


# plot spatial spd and timeslice with spd 500 ----
# get interpolated (limit 2000)
spd_int_500 <- 
  get_interpolated_data(
    data_source =  data_spd_old%>% 
      dplyr::filter(var_name == 500),
    variable = "var_name",
    vars_interpolate = c("age", "value"),
    group_var = "dataset_id",
    method = "linear",
    rule = 1,
    ties = mean,
    age_min = 2000,
    age_max = 8500,
    timestep = 500,
    verbose = TRUE
  )

# select region to plot
select_region <- "Asia"
boundary <-
  data_regional_limits %>% # [config criteria]
  dplyr::filter(region %in% select_region)

# get plot
spd_int_500 %>% 
  filter(dataset_id %in% have_spd_500) %>%
  unnest(data) %>% 
  rename(spd = `500`) %>%
  left_join(data_meta %>% 
              dplyr::select(dataset_id, long, lat, region, climatezone, data_publicity),
            by = "dataset_id") %>%
  filter(!(region == "Latin America" & data_publicity == "private"),
         !region == "Africa") %>%
  dplyr::filter(region == select_region) %>%
  ggplot() +
  ggplot2::borders(
    fill = "grey90",
    colour = "grey90",
  ) +
  ggplot2::coord_sf(
    expand = TRUE,
    ylim = c(boundary$ymin[1], boundary$ymax[1]),
    xlim = c(boundary$xmin[1], boundary$xmax[1])
  ) +
  geom_point(aes(x = long, 
                 y = lat,
                 size = spd,
                 col = spd),
             alpha = 0.5) +
  scale_color_gradient(low="lightblue", high="darkblue")+
  facet_wrap(~age)



#--------------------------------------------------------------#
# 3.0. Summary of spd new 250 data -----
#--------------------------------------------------------------#

# summarise data with spd and no spd ----
data_summary_spd_250 <-
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

# table regions data with spd 250 distance ----
table_region_spd_250 <- 
  data_summary_spd_250 %>%
  group_by(
    region) %>%
  summarise(
    have_spd_250 = sum(have_spd_250), 
    no_spd = sum(no_spd)
  ) %>%
  knitr::kable(., "simple") 

# vector with datasets with spd 250 ----
have_spd_250 <- 
  data_summary_spd_250 %>% 
  filter(have_spd_250 == TRUE) %>%
  pluck("dataset_id")

# plot spatial spd and timeslice with spd 250 ----
# get interpolated 
spd_int_250 <- 
  get_interpolated_data(
  data_source =  data_spd_250_to_fit,
  variable = "var_name",
  vars_interpolate = c("age", "value"),
  group_var = "dataset_id",
  method = "linear",
  rule = 1,
  ties = mean,
  age_min = 0,
  age_max = 8500,
  timestep = 500,
  verbose = TRUE
)

# select region to plot
select_region <- "Asia"
boundary <-
  data_regional_limits %>% # [config criteria]
  dplyr::filter(region %in% select_region)

# get plot
spd_int_250 %>% 
  filter(dataset_id %in% have_spd_250) %>%
  unnest(data) %>% 
  rename(spd = `250`) %>%
  left_join(data_meta %>% 
              dplyr::select(dataset_id, long, lat, region, climatezone, data_publicity),
            by = "dataset_id") %>%
  filter(!(region == "Latin America" & data_publicity == "private"),
         !region == "Africa") %>%
  dplyr::filter(region == select_region) %>%
  ggplot() +
  ggplot2::borders(
    fill = "grey90",
    colour = "grey90",
  ) +
  ggplot2::coord_sf(
    expand = TRUE,
    ylim = c(boundary$ymin[1], boundary$ymax[1]),
    xlim = c(boundary$xmin[1], boundary$xmax[1])
  ) +
  geom_point(aes(x = long, 
                 y = lat,
                 size = spd,
                 col = spd),
             alpha = 0.5) +
  scale_color_gradient(low="lightblue", high="darkblue")+
  facet_wrap(~age)



