#--------------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#
#                    Summary spd and events
#
#                   O. Mottl, V.A. Felde
#                         2024
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

# - load events from targets
data_events <-
  targets::tar_read(
    name = events,
    store = paste0(
      data_storage_path,
      "_targets_data/pipeline_events"
    )
  )

#--------------------------------------------------------------#
# 2.0. Summary of spd data -----
#--------------------------------------------------------------#

# summarise data with spd and no spd ----
sum_spd <-
  data_spd_combine %>%
  left_join(
    data_meta %>%
      dplyr::select(
        dataset_id,
        region,
        climatezone,
        data_publicity
      ),
    by = "dataset_id"
  ) %>%
  filter(
    !(region == "Latin America" & data_publicity == "private"),
    !region == "Africa"
  ) %>%
  mutate(have_spd = purrr::map_lgl(spd,
    .f = ~ any(.x$value > 0)
  )) %>%
  mutate(no_spd = purrr::map_lgl(spd,
    .f = ~ all(.x$value == 0)
  ))

# summarise datasets per region with spd ----
table_spd <-
  sum_spd %>%
  group_by(
    region,
    distance
  ) %>%
  summarise(
    have_spd = sum(have_spd),
    no_spd = sum(no_spd)
  ) %>%
  knitr::kable(., "simple")

#--------------------------------------------------------------#
# 3.0. Summarise sites with events & spd -----
#--------------------------------------------------------------
data_events_filtered <-
  data_events %>%
  unnest(events_updated) %>%
  nest(events = -dataset_id) %>%
  left_join(
    data_meta %>%
      dplyr::select(
        dataset_id,
        region,
        climatezone,
        data_publicity
      ),
    by = "dataset_id"
  ) %>%
  filter(
    !(region == "Latin America" & data_publicity == "private"),
    !region == "Africa"
  )


# combine spd and events ----
table_spd_events <-
  sum_spd %>%
  full_join(
    data_events_filtered %>%
      dplyr::select(
        dataset_id,
        events
      ),
    by = "dataset_id"
  )

# add sum of events type ----
table_sum_spd_events <-
  table_spd_events %>%
  mutate(have_fi = purrr::map_lgl(
    events,
    .f = ~ any(.x$fi > 0)
  )) %>%
  mutate(have_fc = purrr::map_lgl(
    events,
    .f = ~ any(.x$fc > 0)
  )) %>%
  mutate(have_ei = purrr::map_lgl(
    events,
    .f = ~ any(.x$ei > 0)
  )) %>%
  mutate(have_ec = purrr::map_lgl(
    events,
    .f = ~ any(.x$ec > 0)
  )) %>%
  mutate(have_cc = purrr::map_lgl(
    events,
    .f = ~ any(.x$cc > 0)
  )) %>%
  mutate(all_bi = purrr::map_lgl(
    events,
    .f = ~ all(.x$bi == 1)
  )) %>%
  mutate(have_no_impact = purrr::map_lgl(
    events,
    .f = ~ all(.x$no_impact == 1)
  )) %>%
  mutate(have_weak = purrr::map_lgl(
    events,
    .f = ~ any(.x$weak > 0)
  )) %>%
  mutate(have_medium = purrr::map_lgl(
    events,
    .f = ~ any(.x$medium > 0)
  )) %>%
  mutate(have_strong = purrr::map_lgl(
    events,
    .f = ~ any(.x$strong > 0)
  )) %>%
  dplyr::select(
    dataset_id,
    distance,
    region,
    have_spd:no_spd,
    have_fi:have_strong
  )

#--------------------------------------------------------------#
# 3.0. Save  -----
#--------------------------------------------------------------#
# save summary table to data storage ----
RUtilpol::save_latest_file(
  object_to_save = table_sum_spd_events,
  file_name = "table_sum_spd_events",
  dir = paste0(
    data_storage_path,
    "Data/spd/"
  ),
  prefered_format = "rds",
  use_sha = FALSE
)

#---------------------------------------------------------------#
