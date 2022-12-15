library(usethis)
library(tidyverse)
library(targets)

# list R functions and source them
invisible(lapply(
  list.files(
    path = "R/functions",
    pattern = "*.R",
    recursive = TRUE,
    full.names = TRUE
  ),
  source
))

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

# get plot for Individual contribution for each climate zone within continent
plot_summed_circular(
  data_source = data_for_vis,
  group_vars = c("region", "ecozone_koppen_5"),
  col_var = "predictor",
  sel_mode = "individual",
  full_scale = FALSE
)

# get plot for Unique contribution for each continent
plot_summed_circular(
  data_source = data_for_vis,
  group_vars = c("region"),
  col_var = "predictor",
  sel_mode = "unique",
  full_scale = FALSE
)

# get plot for Individual contribution for each continent and color by climate
#   zone
plot_summed_circular(
  data_source = data_for_vis,
  group_vars = c("region", "ecozone_koppen_5"),
  col_var = "ecozone_koppen_5",
  sel_mode = "i_perc_percent",
  full_scale = FALSE
)
