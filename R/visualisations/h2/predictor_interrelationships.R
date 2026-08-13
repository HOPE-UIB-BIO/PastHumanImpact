#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#                 Hypothesis II: Figure 4
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#
library(here)
source(here::here("R/00_Config_file.R"))
source(here::here("R/main_analysis/02_meta_data.R"))

#----------------------------------------------------------#
# 1. Load and extract fitted results -----
#----------------------------------------------------------#
output_h2 <- targets::tar_read(
  name = "output_hvar_h2_spd",
  store = paste0(data_storage_path, "Targets_data/analyses_h2")
)

data_h2_importance <- compute_hvarpart_importance(
  data_source = output_h2 |>
    dplyr::mutate(analysis = "h2_spd"),
  id_cols = c("analysis", "region", "climatezone")
)

summary_h2_zero_truncated <-
  summarise_hvarpart_importance(
    data_importance = data_h2_importance,
    group_vars = c("analysis", "region", "climatezone"),
    profile = "zero_truncated"
  ) |>
  dplyr::mutate(
    region = factor(.data[["region"]], levels = vec_regions)
  ) |>
  prepare_climatezone_factor()

summary_h2_signed <-
  summarise_hvarpart_importance(
    data_importance = data_h2_importance,
    group_vars = c("analysis", "region", "climatezone"),
    profile = "signed"
  ) |>
  dplyr::mutate(
    region = factor(.data[["region"]], levels = vec_regions)
  ) |>
  prepare_climatezone_factor()

summary_h2_balance <-
  compute_hvarpart_importance_balance(
    data_summary = summary_h2_zero_truncated,
    group_vars = c("analysis", "region", "climatezone")
  ) |>
  dplyr::mutate(
    region = factor(.data[["region"]], levels = vec_regions)
  ) |>
  prepare_climatezone_factor()

#----------------------------------------------------------#
# 2. Build hierarchical-composition figures -----
#----------------------------------------------------------#
plots_interrelationships <-
  build_hvarpart_h2_figure(
  output_h2 = output_h2,
  data_meta = data_meta,
  summary_zero_truncated = summary_h2_zero_truncated,
  summary_signed = summary_h2_signed
)

#----------------------------------------------------------#
# 3. Save composite figures -----
#----------------------------------------------------------#
dir.create(
  here::here("Outputs/Figures/H2/Interrelationships"),
  recursive = TRUE,
  showWarnings = FALSE
)

path_main_stem <-
  here::here(
    "Outputs/Figures/H2/Interrelationships/",
    "predictor_interrelationships"
  )

path_untruncated_stem <-
  here::here(
    "Outputs/Figures/H2/Interrelationships/",
    paste0(
      "predictor_interrelationships_",
      "untruncated_hierarchical_contributions"
    )
  )

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ {
    ggplot2::ggsave(
      paste0(path_main_stem, ".", .x),
      plot = plots_interrelationships$main_plot,
      width = image_width_vec[["3col"]] * 0.9,
      height = 200,
      units = image_units,
      bg = "white"
    )

    ggplot2::ggsave(
      paste0(path_untruncated_stem, ".", .x),
      plot = plots_interrelationships$signed_full_range_plot,
      width = image_width_vec[["3col"]] * 0.9,
      height = 200,
      units = image_units,
      bg = "white"
    )
  }
)

readr::write_csv(
  dplyr::bind_rows(
    summary_h2_zero_truncated |>
      dplyr::mutate(profile = "zero_truncated"),
    summary_h2_balance |>
      dplyr::mutate(profile = "balance"),
    summary_h2_signed |>
      dplyr::mutate(profile = "signed")
  ),
  here::here(
    "Outputs/Tables/HVarPart/",
    "predictor_interrelationships_importance_values.csv"
  )
)
