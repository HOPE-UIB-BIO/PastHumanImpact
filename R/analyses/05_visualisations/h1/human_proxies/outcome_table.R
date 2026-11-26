#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#            Single-proxy H1 outcome table figure
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Plots the headline human-minus-climate balance and outcome class of
#   every main human proxy and cell of the single-proxy H1 outcome table.
#   Reads the store analyses_h1/human_proxy_outcomes.

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

store_outcomes <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/human_proxy_outcomes"
  )

runner_h1_proxies <-
  "R/analyses/02_h1_spatiotemporal_hvarpart/human_proxies/00_run.R"

path_figures <-
  here::here("Outputs", "Figures", "H1", "Proxies", "Outcomes")

#----------------------------------------------------------#
# 1. Load the outcome table -----
#----------------------------------------------------------#

data_outcome_summary <-
  load_target_store_value(
    store = store_outcomes,
    target_name = "table_h1_proxy_outcome_summary",
    runner = runner_h1_proxies
  ) |>
  dplyr::filter(.data[["comparison"]] == "main_proxies")

#----------------------------------------------------------#
# 2. Build the figure -----
#----------------------------------------------------------#

plot_outcome_table <-
  plot_human_proxy_outcome_table(data_summary = data_outcome_summary)

#----------------------------------------------------------#
# 3. Save the figure -----
#----------------------------------------------------------#

dir.create(path_figures, recursive = TRUE, showWarnings = FALSE)

save_diagnostic_figure_formats(
  fig_object = plot_outcome_table,
  fig_name = stringr::str_c(
    "human_proxies",
    "human_climate_balance",
    "zero_truncated",
    "outcome_classes",
    sep = "__"
  ),
  path_figures = path_figures,
  width = image_width_vec[["2col"]],
  height = 150,
  units = image_units
)
