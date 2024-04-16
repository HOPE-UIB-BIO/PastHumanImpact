

output_temporal_spd %>% 
  filter(region == "Oceania") %>%
  filter(age == 7000) %>% 
  pluck("varhp") %>% 
  pluck(1)  

dataset <- output_temporal_events %>% 
  filter(region == "Oceania") %>%
  filter(age == 7000) %>% 
  pluck("data_merge") %>% 
  pluck(1) 
  
dataset %>% View()


input <- dataset %>% 
  dplyr::select(n0:density_diversity)



# check dbRDA  


test <- rda(input ~ spd + temp_annual + temp_cold + prec_summer + prec_win,
              data = dataset,
              scale = TRUE)

plot(test)

run_hvarpart()

input <- output_h2 %>% 
  filter(region == "Europe",
  climatezone == "Cold_Without_dry_season_Warm_Summer") %>%
  pluck("data_response_dist") %>% pluck(1)

dataset <- output_h2 %>% 
  filter(region == "Europe",
         climatezone == "Cold_Without_dry_season_Warm_Summer") %>%
  pluck("data_merge") %>% pluck(1) 

test <- dbrda(as.dist(input) ~ spd + temp_annual + temp_cold + prec_summer + prec_win,
            data = dataset,
            scale = TRUE)

plot(test)



##### why does not the hvar work; data looks ok;

dataset_trouble <- output_h2 %>% 
  filter(region == "Europe",
         climatezone == "Cold_Without_dry_season_Warm_Summer")

test_run <- run_hvarpart(dataset_trouble)


test_run2 <- get_varhp(
  data_source = dataset_trouble,
  response_vars = NULL,
  response_dist = NULL,
  data_response_dist = "data_response_dist",
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

## check original function
# prepare data in
dataresp <- as.dist(dataset_trouble$data_response_dist[[1]])

datapreds <- list(
  human = as.data.frame(dataset_trouble$data_merge[[1]] %>% 
    dplyr::select(spd)),
  climate = as.data.frame(dataset_trouble$data_merge[[1]] %>% 
    dplyr::select(temp_annual, temp_cold, prec_summer, prec_win))
  )

# function
varhp <-
  rdacca.hp::rdacca.hp(
    dv = dataresp,
    iv = datapreds,
    type = "adjR2",
    var.part = TRUE
  )


# eror message length of 'dimnames' [2] not equal to array extent

if (is.null(response_dist) & is.null(data_response_dist)) {
  # prepare responses without transformation
  data_resp <-
    data_source %>%
    dplyr::select(
      dplyr::all_of(response_vars)
    ) %>%
    dplyr::select(
      tidyselect:::where(~ any(!is.na(.)))
    )
  
} else if (!is.null(response_dist)) {
  
  # prepare responses with distances
  data_resp <-
    data_source %>%
    dplyr::select(
      dplyr::all_of(response_vars)
    ) %>%
    dplyr::select(
      tidyselect:::where(~ any(!is.na(.)))
    )
  
  data_resp <- vegan::vegdist(data_resp, method = response_dist)
  
} else if (!is.null(data_response_dist)) {
  # if input is already a distance matrix
  data_resp <- data_response_dist
  data_resp <- as.dist(data_resp)
  
  
} else {
  stop("Something went wrong with response variables")
}

# prepare predictors
# if `run_all_predictors` is true then use all variables individually
if (
  isTRUE(run_all_predictors)
) {
  predictor_vars <-
    unlist(predictor_vars) %>%
    rlang::set_names(nm = NULL)
  
  data_preds <-
    test$data_merge[[1]] %>%
    dplyr::select(all_of(predictor_vars)) %>%
    janitor::remove_empty("cols") %>%
    janitor::remove_constant()
  
  output_table_dummy <-
    tibble::tibble(
      predictor = predictor_vars
    )
  
} else {
  
  data_preds <-
    predictor_vars %>%
    purrr::map(
      .x = predictor_vars,
      .f = ~ test$data_merge[[6]] %>%
        dplyr::select(any_of(.x)) %>%
        janitor::remove_empty("cols") %>%
        janitor::remove_constant() %>%
        dplyr::select(
          tidyselect:::where(~ any(. != 0))
        )
    )
  
  # filer out groups with no variables
  data_preds <-
    data_preds[purrr::map_lgl(
      data_preds,
      .f = ~ ncol(.x) > 0
    )]
  
  output_table_dummy <-
    tibble::tibble(
      predictor = names(predictor_vars)
    )
}


# run hvarpar
# should work for both list and just data.frame
varhp <-
  rdacca.hp::rdacca.hp(
    dv = test$data_response_dist[[6]] %>% as.dist(),
    iv = data_preds,
    type = "adjR2",
    var.part = TRUE
  )

# extract relevant summary output
output_table <-
  varhp %>%
  purrr::pluck("Hier.part") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("predictor")

# test significance
if (
  isTRUE(get_significance)
) {
  # should work for both list and just data.frame
  hp_signif <-
    perm_hvarpart(
      dv = data_resp,
      iv = data_preds,
      method = "RDA",
      scale = TRUE,
      type = "adjR2",
      permutations = permutations,
      series = time_series,
      verbose = TRUE,
      ...
    )
  
  # extract relevant summary output
  output_table <-
    output_table %>%
    dplyr::left_join(
      hp_signif,
      by = "Individual"
    )
}


# left join with all predictors (from `output_table_dummy`)
summary_table <-
  output_table_dummy %>%
  dplyr::left_join(
    output_table,
    by = "predictor"
  ) %>%
  # replace all missing values with 0
  dplyr::mutate(
    dplyr::across(
      tidyselect:::where(
        is.numeric
      ),
      ~ tidyr::replace_na(.x, replace = 0)
    )
  )

results <-
  list(
    varhp_output = varhp,
    summary_table = summary_table
  )

return(results)
},
error = function(err) NA
)




summary <- read_rds(paste0(data_storage_path, "Data/spd/table_sum_spd_events_2024-03-11.rds"))
                    