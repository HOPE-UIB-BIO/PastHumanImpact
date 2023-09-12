# MRT analysis 1

#Data upload

output_h1_spatial <-  targets::tar_read(output_hvar_spatial,
                                        store = paste0(
                                          data_storage_path,
                                          "_targets_h1"
                                        ))

data_meta <- targets::tar_read(data_meta,
                               store = paste0(
                                 data_storage_path,
                                 "_targets_h1"
                               ))

# split ecozones
data_meta <- data_meta %>%
  dplyr::mutate(
    sel_classification = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  ) 


# combine with data_meta
data_input <- 
  output_h1_spatial %>%
  inner_join(data_meta %>% 
               dplyr::select(dataset_id, 
                             sel_classification, 
                             region), 
             by = "dataset_id")


# data nested by region + ecozones
regional_data  <- data_input %>%
  dplyr::select(dataset_id, 
                region, 
                sel_classification, 
                data_merge) %>%
  unnest(data_merge) %>%
  nest(data_merge = -c(region, sel_classification))

## RUN MVPART ON RECORDS WITHIN ECOZONES IN REGIONS 
# set.seed(1221)
## AGE as predictor
# region_run1 <- regional_data %>%
#   mutate(mvpart_run1 = purrr::map(data_merge,
#                                   .f = ~run_mvpart(.,
#                                                    preds = "age")))
## SPD as predictor
# set.seed(1222)
# region_run2 <- regional_data %>%
#   mutate(mvpart_run2 = purrr::map(data_merge,
#                                   .f = ~run_mvpart(.,
#                                                    preds = "spd")))
## AGE & SPD as predictor
# set.seed(1223)
# region_run3 <- regional_data %>%
#   mutate(mvpart_run3 = purrr::map(data_merge,
#                                   .f = ~run_mvpart(.,
#                                                    preds = "spd + age")))
## AGE & SPD & CLIMATE VARS AS PREDICTOR
# set.seed(1224)
# region_run4 <- regional_data %>%
#   mutate(mvpart_run4 = purrr::map(data_merge,
#                                   .f = ~run_mvpart(.,
#                                                    preds = "spd + age + temp_annual + temp_cold + prec_summer + prec_win")))

