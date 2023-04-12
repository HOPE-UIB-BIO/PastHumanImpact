# Time analysis
# Run a spatial hvarpart on time bins


# TEST

data_time_analysis <- 
  data_for_hvarpar %>%
  unnest(data_merge) %>%
  drop_na() %>%
  left_join(data_meta %>% 
              dplyr::select(dataset_id, lat, long, region, ecozone_koppen_5),
            by = "dataset_id") %>%
  nest(data_merge = -c(region, ecozone_koppen_5, age)) 



output_time_analysis <- run_hvarpart(
  data_source = data_time_analysis,
  response_vars = c(
    "n0", "n1", "n2",
    "n1_minus_n2", "n2_divided_by_n1", "n1_divided_by_n0",
    "roc",
    "dcca_axis_1"
    #,
    #"density_diversity", "density_turnover"
  ),
  predictor_vars = list(
    human = c("spd"),
    climate = c(
      "temp_annual",
      "temp_cold",
      "prec_summer", 
      "prec_win"
      
    )),
  run_all_predictors = FALSE,
  time_series = FALSE,
  get_significance = TRUE,
  permutations = 19
)

#! Problem while computing `varhp = purrr::map(...)`.
#Caused by error in `match.call()`:
#  ! unused argument (method = "dbrda")



