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

data_h2_importance <- get_hvarpart_importance(
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
  add_climatezone_as_factor()

summary_h2_signed <-
  summarise_hvarpart_importance(
    data_importance = data_h2_importance,
    group_vars = c("analysis", "region", "climatezone"),
    profile = "signed"
  ) |>
  dplyr::mutate(
    region = factor(.data[["region"]], levels = vec_regions)
  ) |>
  add_climatezone_as_factor()

summary_h2_balance <-
  get_hvarpart_importance_balance(
    data_summary = summary_h2_zero_truncated,
    group_vars = c("analysis", "region", "climatezone")
  ) |>
  dplyr::mutate(
    region = factor(.data[["region"]], levels = vec_regions)
  ) |>
  add_climatezone_as_factor()

#----------------------------------------------------------#
# 2. Build main and supplementary figures -----
#----------------------------------------------------------#
figure4 <- build_hvarpart_h2_figure(
  output_h2 = output_h2,
  data_meta = data_meta,
  summary_zero_truncated = summary_h2_zero_truncated,
  summary_signed = summary_h2_signed
)

#----------------------------------------------------------#
# 3. Save composite figures -----
#----------------------------------------------------------#
dir.create(
  here::here("Outputs/Figures/Extended_data_figures/HVarPart"),
  recursive = TRUE,
  showWarnings = FALSE
)

save_figure4 <- function(extension) {
  main_stem <- here::here("Outputs/Figures/Figure4_h2")
  supplementary_stem <- here::here(
    paste0(
      "Outputs/Figures/Extended_data_figures/HVarPart/",
      "Figure4_h2_signed_full_range"
    )
  )

  ggplot2::ggsave(
    paste0(main_stem, ".", extension),
    plot = figure4$main_plot,
    width = image_width_vec[["3col"]] * 0.9,
    height = 200,
    units = image_units,
    bg = "white"
  )
  ggplot2::ggsave(
    paste0(supplementary_stem, ".", extension),
    plot = figure4$signed_full_range_plot,
    width = image_width_vec[["3col"]] * 0.9,
    height = 200,
    units = image_units,
    bg = "white"
  )
}
purrr::walk(c("png", "pdf"), save_figure4)

readr::write_csv(
  dplyr::bind_rows(
    summary_h2_zero_truncated |>
      dplyr::mutate(profile = "zero_truncated"),
    summary_h2_balance |>
      dplyr::mutate(profile = "balance"),
    summary_h2_signed |>
      dplyr::mutate(profile = "signed")
  ),
  here::here("Outputs/Tables/HVarPart/figure4_importance_values.csv")
)
