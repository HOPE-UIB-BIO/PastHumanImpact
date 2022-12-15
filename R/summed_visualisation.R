library(usethis)
library(tidyverse)
library(targets)

# list R functions and source them
lapply(
  list.files(
    path = "R/functions",
    pattern = "*.R",
    recursive = TRUE,
    full.names = TRUE
  ),
  source
) %>%
  invisible()

# get the data
data_to_run <- RUtilpol::get_latest_file("data_to_run")

# get full output
data_varpart_output <-
  data_to_run$data_for_ord %>%
  rlang::set_names(
    nm = data_to_run$dataset_id
  ) %>%
  purrr::map_dfr(
    .id = "dataset_id",
    .f = ~ get_varhp(
      data_source = .x,
      reponse_vars = c(
        "n0", "n1", "n2",
        "n1_minus_n2", "n2_divided_by_n1", "n1_divided_by_n0",
        "roc",
        "dcca_axis_1"
      ),
      predictor_vars = list(
        human = c("spd"),
        climate = c(
          "temp_cold",
          "prec_summer",
          "prec_win",
          "gdm"
        ),
        time = c("age")
      ),
      run_all_predictors = FALSE,
      time_series = TRUE,
      get_significance = FALSE,
      permutations = 99
    ) %>%
      purrr::pluck("summary_table")
  ) %>%
  janitor::clean_names()

data_for_vis <-
  data_varpart_output %>%
  dplyr::inner_join(
    targets::tar_read(data_meta) %>%
      dplyr::select(dataset_id, lat, long, region, ecozone_koppen_5),
    by = "dataset_id"
  )

# get plot for perctenage contribution for a single record
plot_circular(
  data_source = data_for_vis %>%
    dplyr::filter(
      dataset_id == "1758"
    ),
  y_var_name = "i_perc_percent"
)

# get plot for individual contribution for each climate zone within continent
plot_summed_circular(
  data_source = data_for_vis,
  group_vars = c("region", "ecozone_koppen_5"),
  sel_mode = "individual"
)

# get plot for perctenage contribution for each continent on a full scale
#   with error bars for 95-quantiles
plot_summed_circular(
  data_source = data_for_vis,
  group_vars = "region",
  col_var = "predictor",
  sel_mode = "i_perc_percent",
  add_error = "95%",
  full_scale = TRUE
)

# get plot for individual contribution for each continent and color by climate
#   zone. Add polygon for mean values
plot_summed_circular(
  data_source = data_for_vis,
  group_vars = "region",
  col_var = "ecozone_koppen_5",
  sel_mode = "individual",
  add_error = FALSE,
  add_polygon = "mean"
)

# get plot for perentage contribution ffor each continent and color by climate
#   zone. Add error bars by sd and polygon for 95-quantile values
plot_summed_circular(
  data_source = data_for_vis,
  group_vars = "region",
  col_var = "ecozone_koppen_5",
  sel_mode = "i_perc_percent",
  add_error = "sd",
  add_polygon = "95%",
  full_scale = TRUE
)

# get plot for perentage contribution for each climate zone, colored by
#   continent. Add only polygons with mean value, no points
plot_summed_circular(
  data_source = data_for_vis,
  group_vars = "ecozone_koppen_5",
  col_var = "region",
  sel_mode = "i_perc_percent",
  add_error = FALSE,
  add_polygon = "mean",
  point_size = 0,
  full_scale = TRUE
)
