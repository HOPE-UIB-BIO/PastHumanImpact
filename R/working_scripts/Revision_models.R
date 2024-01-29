###########################################################################
#### Rough working script for exploring different scenarios of model input
###########################################################################



#### LOAD DATA FROM TARGETS ####

# climate zones for figures
data_geo_koppen <-
  readr::read_rds(
    paste0(
      data_storage_path,
      "Data/ecoregions2017/data_geo_koppen.rds"
    )
  ) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    sel_classification = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  )

# meta data for records
data_meta <-
  targets::tar_read(
    name = "data_meta",
    store = paste0(
      data_storage_path,
      "_targets_h1"
    )
  )


# subdivide major climate zones in metadata
data_meta <-
  data_meta %>%
  dplyr::mutate(
    sel_classification = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  )

# diversity and dcca estimates made on original data
data_div_dcca <- targets::tar_read(
  name = "data_diversity_and_dcca",
  store = paste0(
    data_storage_path,
    "_targets_h1"
  )
)

# RoC estimates 
data_roc <- targets::tar_read(
  name = "data_roc_for_modelling",
  store = paste0(
    data_storage_path,
    "_targets_h1"
  )
)

#  climate data
data_climate <- targets::tar_read(
  name = "data_climate_for_interpolation",
  store = paste0(
    data_storage_path,
    "_targets_h1"
  )
)

# density estimates
data_density_estimate <- targets::tar_read(
  name = "data_density_estimate",
  store = paste0(
    data_storage_path,
    "_targets_h1"
  )
)

# full spd data
data_spd <- targets::tar_read(
  name = "data_spd_full",
  store = paste0(
    data_storage_path,
    "_targets_h1"
  )
)


data_spd_to_fit <- targets::tar_read(
  name = "data_spd_to_fit",
  store = paste0(
    data_storage_path,
    "_targets_h1"
  )
)

# human events detection data
data_events <- targets::tar_read(
  name = "data_events_to_fit",
  store = paste0(
    data_storage_path,
    "_targets_h1"
  )
)


data_events_temp <- targets::tar_read(
  name = "events_temporal_subset",
  store = paste0(
    data_storage_path,
    "_targets_h1"
  )
)

############# INTERPOLATE DATA AT CONSISTENT TIME SLICES #############


events_int <- get_interpolated_data(
  data_source = data_events,
  variable = "var_name",
  vars_interpolate = c("age", "value"),
  group_var = "dataset_id",
  method = "constant",
  rule = 1,
  ties = "ordered",
  age_min = 0,
  age_max = 12e03,
  timestep = 500,
  verbose = TRUE
)



spd_int <- get_interpolated_data(
  data_source = data_spd_to_fit,
  variable = "var_name",
  vars_interpolate = c("age", "value"),
  group_var = "dataset_id",
  method = "linear",
  rule = 1,
  ties = mean,
  age_min = 0,
  age_max = 12e03,
  timestep = 500,
  verbose = TRUE
)

data_climate_int <- get_interpolated_data(
  data_source = data_climate,
  variable = "var_name",
  vars_interpolate = c("age", "value"),
  group_var = "dataset_id",
  method = "linear",
  rule = 1,
  ties = mean,
  age_min = 0,
  age_max = 12e03,
  timestep = 500,
  verbose = TRUE
)


div_dcca_int <- get_interpolated_data(
  data_source = data_div_dcca,
  variable = "var_name",
  vars_interpolate = c("age", "value"),
  group_var = "dataset_id",
  method = "linear",
  rule = 1,
  ties = mean,
  age_min = 0,
  age_max = 12e03,
  timestep = 500,
  verbose = TRUE
)

data_roc_int <- get_interpolated_data(
  data_source = data_roc,
  variable = "var_name",
  vars_interpolate = c("age", "value"),
  group_var = "dataset_id",
  method = "linear",
  rule = 1,
  ties = mean,
  age_min = 0,
  age_max = 12e03,
  timestep = 500,
  verbose = TRUE
)

# NB This is done very crude and roughly, the age_min and age_max are not the same for all datasets, and need to be adjusted. Now the interpolated values goes towards 0 at either end of the time series beyond the time in each record, which is not ideal. But it was done with purpose as one of the test was to add all predictors in without data being discarded.

############### COMBINE HUMAN RECONSTRUCTION DATA ################


human_predictors <- spd_int %>%
  rename(spd = data) %>%
  full_join(events_int %>% 
              rename(events = data), by = "dataset_id") %>%
  left_join(data_spd, by = "dataset_id")%>% 
  left_join(data_meta %>% 
              dplyr::select(dataset_id, lat, long, region, sel_classification))


# dummy data to be replaced with missing data
dummy_data_events <- tibble(age = seq(0,12000, by = 500)) %>%
  mutate(bi = 0,
         fc = 0,
         es = 0,
         fi = 0,
         ec = 0,
         cc = 0,
         no_impact = 0,
         weak = 0,
         medium = 0,
         strong = 0,
         ei = 0)

dummy_data_spd <- tibble(age = seq(0,12000, by = 500)) %>%
  mutate("250" = 0)

human_predictors_all <- human_predictors %>%
  mutate(events = purrr::map(events, .f = function(x){
    if(is.null(x)){
      x <- dummy_data_events
    } else {
      x <- x
    }
  }))%>%
  mutate(spd = purrr::map(spd.x, .f = function(x){
    if(is.null(x)){
      x <- dummy_data_spd
    } else {
      x <- x
    }
  })) %>%
  dplyr::select(-spd.x, -spd.y, -best_dist, -spd_from_events)

human_predictors_all <- 
  human_predictors_all %>%
  mutate(human = purrr::map2(spd, events,
                             .f = function(.x, .y){
                               table <-  .x %>%
                                 dplyr::select(age, "250") %>%
                                 left_join(.y %>% remove_empty(), 
                                           by = "age") %>%
                                 drop_na()
                               return(table)
                             })) 



# COMBINE HUMAN PREDICTORS WITH PROPERTIES AND CLIMATE DATA

properties_data <- get_data_properties(
  data_source_diversity = div_dcca_int,
  data_source_roc = data_roc_int,
  data_source_density = data_density_estimate,
  used_rescale = TRUE)

data_all <- properties_data %>%
  left_join(human_predictors_all, by = "dataset_id") %>%
  left_join(data_climate_int %>% 
              rename(climate = data), by = "dataset_id") 

data_all_merged <- data_all %>%
  filter(!region == "Africa") %>%
  mutate(data_merge_all = purrr::pmap(list(data_merge, 
                                           human, 
                                           climate),
                                      .f = ~left_join(..1, 
                                                      ..2, 
                                                      by = "age") %>%
                                        left_join(..3, 
                                                  by = "age") )) %>% 
  dplyr::select(-c(data_merge, human, events, climate)) %>%
  rename(data_merge = data_merge_all) %>%
  mutate(data_merge = purrr::map(data_merge, 
                                 .f = function(x){ x %>% 
                                     remove_empty() %>% 
                                     drop_na() %>% 
                                     return()
                                 }
  ))

################## REVISION TEST RUN ######################


# function to run scenarios 
run_scenario <- function(sel_region = "Europe",
                         age_min = 0,
                         age_max = 9000,
                         human_pred = human_event,
                         climate_pred = climate_pred,
                         time = NULL,
                         response_vars = response_vars
                         ) {

data_to_run <- 
  data_all_merged %>%
  filter(region == sel_region) %>%
  mutate(data_merge = purrr::map(data_merge, .f = function(x){
    x %>% filter(age >= age_min & age <=age_max) %>% return()
  }))



# 1. SPATIAL HVAR RECORD WISE
hvar_spatial <- run_hvarpart(
  data_source = data_to_run,
  response_vars = response_vars,
  predictor_vars = list(
    human = human_pred,
    climate = climate_pred,
    time = time
  ),
  run_all_predictors = FALSE,
  time_series = TRUE,
  get_significance = FALSE,
  permutations = 999
)

# 2. TEMPORAL HVAR PER TIME SLICE

data_to_run_temporal <- 
  data_to_run %>%
  filter(!dataset_id == 	
           "COLE_etal_2015_CPL") %>%
  unnest(data_merge) %>%
  nest(data_merge = -c(region, age)) %>%
  mutate(n_samples = purrr::map_dbl(data_merge, 
                                    .f = function(x){ 
                                      nrow(x) %>% 
                                        return()})) 


hvar_temporal <- run_hvarpart(
  data_source = data_to_run_temporal,
  response_vars = response_vars,
  predictor_vars = list(
    human = human_pred,
    climate = climate_pred
  ),
  run_all_predictors = FALSE,
  time_series = FALSE,
  get_significance = FALSE,
  permutations = 199
)



####### GET SUMMARY

##### spatial summary ########
spatial_summary <- 
  hvar_spatial %>%
  dplyr::mutate(
    summary_table = purrr::map(
      .x = varhp,
      .f = ~ purrr::pluck(.x, "summary_table")
    )
  ) %>%
  tidyr::unnest(summary_table) %>%
  dplyr::select(-c(data_merge, varhp)) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = Unique,
      .fns = ~ replace(., .x < 0, 0.0001)
    )
  ) %>% # negative variances can be ignored
  group_by(
    region, sel_classification
  ) %>%
  dplyr::mutate(
    n_records = length(unique(dataset_id))
  ) %>%
  dplyr::ungroup() %>%
  janitor::clean_names()


spatial_summary <-
  spatial_summary %>%
  group_by(dataset_id) %>%
  mutate(sum_importance = sum(individual),
         ratio_unique = unique/sum_importance,
         ratio_ind = individual/sum_importance) %>%
  ungroup()




r2_summary_spatial <-
  spatial_summary %>%
  dplyr::group_by(
    dplyr::across(
      dplyr::all_of(c("predictor","region", "sel_classification"))
    )
  ) %>%
  dplyr::summarise(
    .groups = "drop",
    dplyr::across(
      dplyr::all_of(c("ratio_unique", "ratio_ind")),
      list(
        mean = ~ mean(.x, na.rm = TRUE)
        
      )
    )
  )

# Get median values H1 spatial
summary_spatial_mean <-
  r2_summary_spatial %>%
  tidyr::pivot_longer(
    dplyr::ends_with("mean"),
    names_to = "importance_type",
    values_to = "ratio"
  ) 


######## temporal summary ########
temporal_summary <-
  hvar_temporal %>%
  dplyr::mutate(
    summary_table = purrr::map(
      .x = varhp,
      .f = ~ .x %>%
        purrr::pluck("summary_table")
    )
  ) %>%
  tidyr::unnest(summary_table) %>%
  
  dplyr::mutate(
    dplyr::across(
      .cols = Unique,
      .fns = ~ replace(., .x < 0, 0.0001)
    )
  ) %>%
  dplyr::select(-c(data_merge, varhp)) %>%
  dplyr::ungroup() %>%
  janitor::clean_names() 

temporal_summary <- temporal_summary %>%
  group_by(age) %>%
  mutate(sum_importance = sum(individual),
         ratio_unique = unique/sum_importance,
         ratio_ind = individual/sum_importance) %>%
  ungroup()


# Get adjusted R2 summary tables H1 temporal
r2_summary_temporal <-
  temporal_summary %>%
  dplyr::group_by(
    dplyr::across(
      dplyr::all_of(c("age","region", "predictor"))
    )
  ) %>%
  dplyr::summarise(
    .groups = "drop",
    dplyr::across(
      dplyr::all_of(c("ratio_unique", "ratio_ind")),
      list(
        mean = ~ mean(.x, na.rm = TRUE)
        
      )
    )
  )

summary_temporal_mean <-
  r2_summary_temporal %>%
  tidyr::pivot_longer(
    dplyr::ends_with("mean"),
    names_to = "importance_type",
    values_to = "ratio"
  ) 

results <- list(
  spatial_summary = spatial_summary,
  temporal_summary = temporal_summary,
  summary_spatial_mean = summary_spatial_mean,
  summary_temporal_mean = summary_temporal_mean
)

return(results)
}


# plot scenarios events and spd combined
plot_temporal_combined <- function(scenario4, scenario1) {
  
  data_source_temporal <- scenario4$summary_temporal_mean %>%
    mutate(human_pred = "spd") %>%
    full_join(
      scenario1$summary_temporal_mean %>%
        mutate(human_pred = "events"),
      by = c("age", "region", "predictor", "importance_type", "human_pred", "ratio")
    ) %>%
    mutate(predictor = factor(predictor, levels = c("human", "climate"))) 
  
  figure_temporal <-
    data_source_temporal %>% 
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = data_source_temporal %>%
        dplyr::filter(
          importance_type == "ratio_unique_mean"
        ),
      mapping = ggplot2::aes(
        x = as.factor(age / 1000),
        y = get("ratio"),
        fill = human_pred
      ),
      stat = "identity",
      width = .6,
      alpha = 1,
      position = ggplot2::position_dodge2(
        width = 0.8,
        preserve = "single"
      ),
      show.legend = FALSE
    ) +
    ggplot2::geom_bar(
      data = data_source_temporal %>%
        dplyr::filter(
          importance_type == "ratio_ind_mean"
        ),
      ggplot2::aes(
        x = as.factor(age / 1000),
        y = get("ratio"),
        fill = human_pred
      ),
      stat = "identity",
      width = .6,
      alpha = 0.4,
      position = ggplot2::position_dodge2(
        width = 0.8,
        preserve = "single"
      ),
      show.legend = FALSE
    ) +
    scale_y_continuous(
      limits = c(-0.2, 1),
      breaks = seq(-0.2, 1, 0.2)
    ) +
    scale_x_discrete(limit = rev) +
    
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      line = ggplot2::element_line(linewidth = 0.01),
      text = ggplot2::element_text(size = 15, color = "grey30"),
      # plot.margin = grid::unit(c(0, 0, 20, 20), "mm"),
      # axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(hjust = 1),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 12, color = "grey30"),
      axis.line = ggplot2::element_blank()
    )+
    facet_wrap(~ predictor, ncol = 1) +
    labs(x = "Age (ka) BP", y = "Ratio of importance")  
  
  return(figure_temporal)
}

# plot combined spatial and temporal of scenario runs below
plot_spatio_temporal <- function(data_source) {
  data_source_spatial <-
    data_source$summary_spatial_mean %>% 
    filter(region == sel_region) %>%
    mutate(predictor = factor(predictor, levels = c("time", "climate", "human")))
  
  figure_spatial <- 
    data_source_spatial %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = get("ratio"),
        y = get("predictor"),
        fill = get("sel_classification")
      )
    ) +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_rect(
        fill = "white",
        color = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = "white",
        color = NA
      ),
      panel.border = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
      panel.spacing = grid::unit(c(0, 0, 0, 0), "mm"),
      text = ggplot2::element_text(size = 15, color = "grey30")
    ) +
    ggplot2::scale_fill_manual(
      values = palette_ecozones,
      drop = FALSE
    ) +
    
    ggplot2::geom_col(
      data = data_source_spatial %>%
        dplyr::filter(
          grepl(
            "ratio_unique_mean",
            importance_type
          )
        ),
      width = 0.6,
      position = ggplot2::position_dodge2(
        width = 0.8,
        preserve = "single"
      ),
      alpha = 1
    ) +
    ggplot2::geom_col(
      data = data_source_spatial %>%
        dplyr::filter(grepl(
          "ratio_ind_mean",
          importance_type
        )),
      width = .6,
      position = ggplot2::position_dodge2(
        width = 0.8,
        preserve = "single"
      ),
      alpha = 0.4
    )
  
  data_source_temporal <- 
    data_source$summary_temporal_mean %>%
    mutate(predictor = factor(predictor, levels = c("human", "climate"))) 
  
  figure_temporal <-
    data_source_temporal %>% 
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = data_source_temporal %>%
        dplyr::filter(
          importance_type == "ratio_unique_mean"
        ),
      mapping = ggplot2::aes(
        x = as.factor(age / 1000),
        y = get("ratio"),
        fill = predictor
      ),
      stat = "identity",
      width = .6,
      alpha = 1,
      position = ggplot2::position_dodge2(
        width = 0.8,
        preserve = "single"
      ),
      show.legend = FALSE
    ) +
    ggplot2::geom_bar(
      data = data_source_temporal %>%
        dplyr::filter(
          importance_type == "ratio_ind_mean"
        ),
      ggplot2::aes(
        x = as.factor(age / 1000),
        y = get("ratio"),
        fill = predictor
      ),
      stat = "identity",
      width = .6,
      alpha = 0.4,
      position = ggplot2::position_dodge2(
        width = 0.8,
        preserve = "single"
      ),
      show.legend = FALSE
    ) +
    scale_x_discrete(limit = rev) +
    
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      line = ggplot2::element_line(linewidth = 0.01),
      text = ggplot2::element_text(size = 15, color = "grey30"),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
      axis.text.x = ggplot2::element_text(size = 10, color = "grey30"),
      axis.text.y = ggplot2::element_text(hjust = 1),
      axis.ticks.x = ggplot2::element_blank(),
      # axis.title = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank()
    ) +
    facet_wrap(~ predictor, ncol = 1) +
    labs(x = "Age (ka) BP", y = "Ratio of importance")
  
  combine_fig <- ggarrange(figure_spatial, 
                           figure_temporal, 
                           labels = c("A", "B"),
                           ncol = 2, 
                           nrow = 1)
  return(combine_fig)
  
  
}


########## RUN SCENARIOS ##########

# model input

# events
human_events_eu <-  c("fi", "fc", "ec", "cc")
human_events_na <- c("fc", "es")
human_events_asia <- c("fi", "fc", "ei")
human_events_la <- c("weak", "medium", "strong")
human_events_ippd <- c("weak", "medium", "strong")

# distance
human_distance <- c("250")

# climate
climate_pred <- c("temp_annual", "temp_cold", "prec_summer", "prec_win")

# responses
response_vars <- c(
  "n0", "n1", "n2",
  "n1_minus_n2", "n2_divided_by_n1", "n1_divided_by_n0",
  "roc",
  "dcca_axis_1",
  "density_diversity", "density_turnover"
)

diversity_vars <-  c(
  "n0", "n1", "n2",
  "n1_minus_n2", "n2_divided_by_n1", "n1_divided_by_n0", 
  "density_diversity"
)

turnover_vars <- c(
  "roc",
  "dcca_axis_1", 
  "density_turnover"
)

select_vars <-  c(
  "n0", 
  "n2_divided_by_n1", 
  "roc",
  "dcca_axis_1"
)


# Europe

# human predictors
Europe_scenario1 <- run_scenario(
  sel_region = "Europe",
  age_min = 2500,
  age_max = 9000,
  human_pred = human_distance,
  climate_pred = climate_pred,
  time = c("age")
)

Europe_scenario4 <- run_scenario(
  sel_region = "Europe",
  age_min = 0,
  age_max = 9000,
  human_pred = human_events_eu,
  climate_pred = climate_pred,
  time = c("age")
)

plot_temporal_combined(Europe_scenario4,
                       Europe_scenario1)

# responses
Europe_scenario_all_response <- run_scenario(
  sel_region = "Europe",
  age_min = 2500,
  age_max = 9000,
  human_pred = human_distance,
  climate_pred = climate_pred,
  response_vars = response_vars,
  time = c("age")
)

plot_spatio_temporal(Europe_scenario_all_response)
Europe_scenario_turnover <- run_scenario(
  sel_region = "Europe",
  age_min = 2500,
  age_max = 9000,
  human_pred = human_distance,
  climate_pred = climate_pred,
  response_vars = turnover_vars,
  time = c("age")
)

plot_spatio_temporal(Europe_scenario_turnover)


Europe_scenario_diversity <- run_scenario(
  sel_region = "Europe",
  age_min = 2500,
  age_max = 9000,
  human_pred = human_distance,
  climate_pred = climate_pred,
  response_vars = diversity_vars,
  time = c("age")
)

plot_spatio_temporal(Europe_scenario_diversity)

Europe_scenario_select <- run_scenario(
  sel_region = "Europe",
  age_min = 2500,
  age_max = 9000,
  human_pred = human_distance,
  climate_pred = climate_pred,
  response_vars = select_vars,
  time = c("age")
)

plot_spatio_temporal(Europe_scenario_select)


# North America

# predictor scenarios
NA_scenario1 <- run_scenario(
  sel_region = "North America",
  age_min = 2500,
  age_max = 9000,
  human_pred = human_distance,
  climate_pred = climate_pred,
  time = c("age")
)
NA_scenario4 <- run_scenario(
  sel_region = "North America",
  age_min = 0,
  age_max = 9000,
  human_pred = human_events_na,
  climate_pred = climate_pred,
  time = c("age")
)

plot_temporal_combined(NA_scenario4,
                       NA_scenario1)


# Latin America

# predictor scenarios
LA_scenario1 <- run_scenario(
  sel_region = "Latin America",
  age_min = 2500,
  age_max = 9000,
  human_pred = human_distance,
  climate_pred = climate_pred,
  time = c("age")
)

LA_scenario4 <- run_scenario(
  sel_region = "Latin America",
  age_min = 0,
  age_max = 9000,
  human_pred = human_events_la,
  climate_pred = climate_pred,
  time = c("age")
)

plot_temporal_combined(LA_scenario4,
                       LA_scenario1)


# Asia

# predictor scenarios
A_scenario1 <- run_scenario(
  sel_region = "Asia",
  age_min = 2500,
  age_max = 9000,
  human_pred = human_distance,
  climate_pred = climate_pred,
  time = c("age")
)

A_scenario4 <- run_scenario(
  sel_region = "Asia",
  age_min = 0,
  age_max = 9000,
  human_pred = human_events_asia,
  climate_pred = climate_pred,
  time = c("age")
)

plot_temporal_combined(A_scenario4,
                       A_scenario1)

# Oceania
# predictor scenarios

O_scenario1 <- run_scenario(
  sel_region = "Oceania",
  age_min = 2500,
  age_max = 9000,
  human_pred = human_distance,
  climate_pred = climate_pred,
  time = c("age")
)

O_scenario4 <- run_scenario(
  sel_region = "Oceania",
  age_min = 0,
  age_max = 9000,
  human_pred = human_events_ippd,
  climate_pred = climate_pred,
  time = c("age")
)

plot_temporal_combined(O_scenario4,
                       O_scenario1)



########### Figure of human events for EUROPE ###################

set_palette <-
  c(
    "bi" = "grey60",
    "fi" = "#c99000",
    "ei" = "#a17400",
    "ec"= "#7b5800",
    "cc" = "#573e00",
    "fc" = "#00c92b",
    "es" = "#c9009e",
    "weak"  = "#9b541b",
    "medium"  = "#5d261a",
    "strong" =  "#1f0000",
    "no_impact" = "grey60"
    
  )

fig_human_events <- 
  human_predictors %>%
  filter(region == "Europe") %>%
  unnest(events) %>%
  pivot_longer(cols = c("bi", "fi", "fc","ec" ,"cc"), 
               names_to = "events", 
               values_to = "type") %>%
  ggplot(aes(x = age, y = type, col = events)) +
  geom_point() +
  scale_colour_manual(values = set_palette, drop = TRUE) +
  geom_smooth(
    method = "gam",
    method.args = list(
      family = "binomial"),
    se = FALSE) +
  scale_x_reverse(limit = c(9000,0)) +
  labs(y = "human impact", x = "years BP")
# facet_wrap(~sel_classification) 



# ### Figure regional maps
# 
# region_map <- 
#   get_map_region(
#     rasterdata = data_geo_koppen,
#     select_region = sel_region,
#     sel_palette = palette_ecozones, # [config criteria]
#     sel_alpha = 0.5
#   )
# 
# 
# region_map_points <- 
#   region_map +
#   geom_point(data = spatial_summary %>% 
#                filter(region == sel_region,
#                       predictor == "human"),
#              aes(x = long,
#                  y = lat,
#                  size = individual),
#              alpha=0.5) +
#   scale_size_continuous(range=c(0,5),
#                         limits = c(0, 1)) +
#   theme(legend.position = "none")



############# FOR VISUALISATION OF HUMAN DATA  #####################
##### COMPARE SPD CURVES BY DISTANCES, SPD INPUT USED, AND EVENTS

# dummy_events <- tibble(age = seq(0, 12000, by = 500),
#                            bi = 1,
#                            fc = 0,
#                            fi = 0,
#                            ec = 0,
#                            cc = 0
#                            )
# 
# human_predictors_europe <- 
#   human_predictors %>%
#   filter(region == "Europe") %>%
#   mutate(events = purrr::map(events, .f = function(x) {
#     if (is.null(x)) {
#       dummy_events
#     } else {
#       x
#     }
#   })) 
#                                  
#                                


# plot_events_spd <- function(input_spd, input_events, spd_used, id) {
#   
#   events_fig <- 
#     input_events %>%
#     pivot_longer(cols = any_of(c("bi", "fi", "fc", "ec", "cc")), names_to = "events", values_to = "presence") %>%
#     replace_na(list(presence = 0)) %>%
#     ggplot(aes(y = age, x = presence, color = events)) +
#     scale_y_reverse()+
#     scale_x_continuous(limits = c(0,1), breaks = c(0,1))+
#     abline(h = 2000, col = "grey")+
#     geom_point() +
#     facet_wrap(~events, ncol = 5)  +
#     labs(y = "")
#   
#   spd_distances <- 
#     input_spd %>%
#     pivot_longer("5":"500", names_to = "distance", values_to = "spd") %>%
#     replace_na(list(presence = 0)) %>%
#     ggplot(aes(y = age, x = spd, color = distance)) +
#     abline(h = 2000, col = "grey")+
#     scale_y_reverse()+
#     geom_path() +
#     labs(y = "calibrated ages")
#     
#   spd_fig <- 
#     spd_used %>%
#     replace_na(list(presence = 0)) %>%
#     ggplot(aes(y = age, x = spd )) +
#     scale_y_reverse()+
#     abline(h = 2000, col = "grey")+
#     geom_path() +
#     labs(y = "")
#   
#   both <- ggarrange(spd_distances, spd_fig, events_fig, nrow = 1)
#  
#   ggsave(paste0("past_humans/", id, ".png"), both, width = 10, height = 5)
#   
# }
# 
# 
# purrr::pmap(list(human_predictors_europe$spd.x,
#                  human_predictors_europe$events,
#                  human_predictors_europe$spd.y, 
#                  human_predictors_europe$dataset_id),  
#             .f = ~plot_events_spd(input_spd =..1,
#                                   input_events = ..2,
#                                   spd_used = ..3,
#                                   id = ..4))


#Human impact vector
# input_test <- human_predictors_europe %>%
#   filter(dataset_id == 20223)
# 
# #source area ~200 km2
# #theoretical distance radiocarbon dates 25 km ~ 50 km
# 
# 
# input <- input_test$spd.x[[1]] %>%
#   dplyr::select(age, "100") %>%
# left_join(input_test$events[[1]] %>%
#   remove_empty()) %>%
#   drop_na()
# 
# input <- input %>%
#   dplyr::select(-bi) 
# 
# pca_test <- vegan::rda(input[,3:6], scale = TRUE)
# 
# pca_sc1 <- scores(pca_test, display = "sites", choices = 1)
# 
# data.frame(pca_sc1) %>%
#   mutate(age = input$age) %>%
#   ggplot(aes(x = PC1, y = age)) +
#   geom_path() +
#   scale_y_reverse()+
#   geom_point() +
#   labs(y = "calibrated ages")

# human_predictors <- human_predictors %>%
#   mutate(human = purrr::pmap(list(spd, events),
#                              .f = ~full_join(..1, ..2, by = "age"))) %>%
#   dplyr::select(-spd, -events) 

# predictors <- human_predictors %>%
#   full_join(data_climate_int %>% rename(climate = data), by = "dataset_id") %>%  
#   mutate(predictors = purrr::pmap(list(human, climate),
#                                                                                                              .f = ~inner_join(..1, ..2, by = "age") %>%
#                                     rename(spd = "100"))) %>%
#   dplyr::select(-human, -climate) 

# input_test$spd.x[[1]] %>%
#   dplyr::select(age, "100") %>%
#   left_join(input_test$events[[1]] %>%
#               remove_empty()) %>%
#   drop_na()