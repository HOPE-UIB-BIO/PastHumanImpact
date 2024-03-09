#--------------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#                    
#                
#           Revision H2 with data spd = TRUE
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

# Load spd data
data_spd_250_to_fit <- 
  targets::tar_read(
    name = "data_spd_to_fit",
    store = paste0(
      data_storage_path,
      "_targets_data/pipeline_predictors"
    )
  )

# - Load data paps
data_properties <-
  targets::tar_read(
    name = "data_properties",
    store = paste0(
      data_storage_path,
      "_targets_data/pipeline_paps"
    )
  )


# Load predictors general trends
mod_predicted <- targets::tar_read(
  name = "mod_predicted_merged",
  store = paste0(
    data_storage_path,
    "_targets_data/analyses_h2"
  )
)


#--------------------------------------------------------------#
# 1. filter data with spd
#--------------------------------------------------------------#

# summary data with spd and no spd ----
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

datasets_with_spd <- data_summary_spd %>%
  dplyr::filter(have_spd_250 == TRUE) %>%
  pluck("dataset_id")

datasets_with_spd %>% length()  

# filter data paps ----
data_properties_filtered_2000 <-
  get_data_filtered(
    data_source = data_properties,
    data_meta = data_meta,
    age_from = 2000,
    age_to = 8500,
    remove_private = TRUE
  )  %>%
  dplyr::filter(dataset_id %in% datasets_with_spd)


# get data m2 ----
data_m2_filtered_2000 <-
  get_data_m2(
    data_source = data_properties_filtered_2000,
    data_meta = data_meta,
    min_samples = 5,
    select_vars = c(
      "dataset_id",
      "age",
      "n0", 
      "n1", 
      "n2",
      "n1_minus_n2", 
      "n2_divided_by_n1", 
      "n1_divided_by_n0",
      "roc",
      "dcca_axis_1",
      "density_diversity", 
      "density_turnover"
    )
  )

# prepare data h2 ----
data_hvar_h2 <- get_data_for_h2_hvar(
  data_m2 = data_m2_filtered_2000,
  data_predictors = mod_predicted
)

# run multidimensional shifts h2 ----
results_h2_2000 <- run_hvarpart(
  data_source = data_hvar_h2,
  response_vars = NULL,
  response_dist = NULL,
  data_response_dist = "m2",
  predictor_vars = list(
    human = c("spd"),
    climate = c(
      "temp_annual",
      "temp_cold",
      "prec_summer",
      "prec_win"
    )
  ),
  run_all_predictors = FALSE,
  time_series = FALSE,
  get_significance = FALSE
)

# create summary table for h2 ----
summary_h2_2000 <- 
  results_h2_2000 %>%
  dplyr::mutate(
    summary_table = purrr::map(
      .x = varhp,
      .f = ~ .x %>%
        purrr::pluck("summary_table")
    )
  ) %>%
  tidyr::unnest(summary_table) %>%
  dplyr::select(-c(data_merge, data_response_dist, varhp)) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = Unique,
      .fns = ~ replace(., .x < 0, 0.0001)
    )
  ) %>% # negative variances can be ignored  
  janitor::clean_names() %>%
  group_by(
    region, climatezone
  ) %>%
  mutate(sum_importance = sum(individual),
         ratio_unique = unique/sum_importance,
         ratio_ind = individual/sum_importance) %>%
  ungroup()


# long table summary ----
summary_h2_long_2000 <-
  summary_h2_2000 %>%
  dplyr::group_by(
    region,
    climatezone,
    predictor
  ) %>% 
  #summarise by model weight
  dplyr::summarise(
    .groups = "drop",
    dplyr::across(
      dplyr::all_of(
        c("ratio_unique",
          "ratio_ind")
      ),
      list(
        wmean = ~ weighted.mean(
          x = .x,
          w = sum_importance,
          na.rm = TRUE)
      )
    )
  ) %>%
  tidyr::pivot_longer(
    dplyr::starts_with("ratio"),
    names_to = "importance_type",
    values_to = "ratio"
  ) 



