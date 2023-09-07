###MVPARTwrap package
# tools for mvpart 

# url <- "http://cran.r-project.org/src/contrib/Archive/MVPARTwrap/MVPARTwrap_0.1-9.2.tar.gz"
# install.packages(url)
# 


library(MVPARTwrap)

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


# add constrained ecoystem scores with spds; NAs when empty
output_h1_spatial <- 
  output_h1_spatial %>%
  mutate(constrained_scores = purrr::map(data_merge,
                                         .f =  possibly(get_scores_constrained_spd,
                                                        otherwise = NA_real_)))
  

test$constrained_scores[[1]]

# combine with data_meta
data_input <- 
  output_h1_spatial %>%
  inner_join(data_meta %>% 
               dplyr::select(dataset_id, 
                             sel_classification, 
                             region), 
             by = "dataset_id")

# 1.2 data nested by region + ecozones
regional_data  <- data_input %>%
  dplyr::select(dataset_id, 
                region, 
                sel_classification, 
                data_merge) %>%
  unnest(data_merge) %>%
  nest(data_merge = -c(region, sel_classification))




# mvpart validation analysis
set.seed(1221)
region_run1 <- regional_data %>%
  mutate(mvpart_run1 = purrr::map(data_merge,
                                  .f = ~run_mvpart(.,
                                                   preds = "age")))

set.seed(1222)
region_run2 <- regional_data %>%
  mutate(mvpart_run2 = purrr::map(data_merge,
                                  .f = ~run_mvpart(.,
                                                   preds = "spd")))
set.seed(1223)
region_run3 <- regional_data %>%
  mutate(mvpart_run3 = purrr::map(data_merge,
                                  .f = ~run_mvpart(.,
                                                   preds = "spd + age")))
set.seed(1224)
region_run4 <- regional_data %>%
  mutate(mvpart_run4 = purrr::map(data_merge,
                                  .f = ~run_mvpart(.,
                                                   preds = "spd + age + temp_annual + temp_cold + prec_summer + prec_win")))



# mvpart on constrained scores

data_input2 <- 
  dplyr::select(dataset_id, 
                region, 
                sel_classification, 
                constrained_scores) 
# %>%
#   unnest(constrained_scores) %>%
#   nest(constrained_scores = -c(region, sel_classification))

# pivot_wider cap per dataset_id
data_input2 %>%
  filter(region == "Europe") %>%
  mutate(mvpart_run1 = purrr::map(data_merge,
                                  .f = ~run_mvpart(.,
                                                   preds = "age")))
