#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#                 Hypothesis I: Figure 2
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
output_spatial_spd <- targets::tar_read(
  name = "output_spatial_spd",
  store = paste0(data_storage_path, "Targets_data/analyses_h1")
)

data_importance <- get_hvarpart_importance(
  data_source = output_spatial_spd |>
    dplyr::left_join(
      data_meta |>
        dplyr::select(dataset_id, region, climatezone),
      by = "dataset_id"
    ) |>
    dplyr::mutate(analysis = "spatial_spd"),
  id_cols = c("analysis", "dataset_id", "region", "climatezone")
)

data_geo_koppen <-
  readr::read_rds(
    paste0(data_storage_path, "Spatial/Climatezones/data_geo_koppen.rds")
  ) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    climatezone = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 %in% c("Cold", "Temperate") ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  ) |>
  add_climatezone_as_factor()

#----------------------------------------------------------#
# 2. Build balance main and signed supplementary figures -----
#----------------------------------------------------------#
figure2_balance <- plot_hvarpart_spatial_balance(
  data_importance = data_importance,
  data_meta = data_meta,
  data_geo_koppen = data_geo_koppen
)
figure2_signed <- plot_hvarpart_spatial_signed(
  data_importance = data_importance,
  data_meta = data_meta,
  data_geo_koppen = data_geo_koppen
)

#----------------------------------------------------------#
# 3. Save figures and source tables -----
#----------------------------------------------------------#
dir.create(
  here::here("Outputs/Tables/HVarPart"),
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  here::here("Outputs/Figures/Extended_data_figures/HVarPart"),
  recursive = TRUE,
  showWarnings = FALSE
)

save_figure2 <- function(extension) {
  ggplot2::ggsave(
    paste0(
      here::here("Outputs/Figures/Figure2_h1_spatial"),
      ".",
      extension
    ),
    plot = figure2_balance$plot,
    width = image_width_vec[["3col"]],
    height = 170,
    units = image_units,
    bg = "white"
  )
  ggplot2::ggsave(
    paste0(
      here::here(
        paste0(
          "Outputs/Figures/Extended_data_figures/HVarPart/",
          "Figure2_h1_spatial_signed_full_range"
        )
      ),
      ".",
      extension
    ),
    plot = figure2_signed$plot,
    width = image_width_vec[["3col"]],
    height = 180,
    units = image_units,
    bg = "white"
  )
}
purrr::walk(c("png", "pdf"), save_figure2)

readr::write_csv(
  figure2_signed$record_values,
  here::here("Outputs/Tables/HVarPart/figure2_record_values.csv")
)
readr::write_csv(
  figure2_signed$climatezone_values,
  here::here("Outputs/Tables/HVarPart/figure2_pooled_values.csv")
)
readr::write_csv(
  figure2_signed$region_values,
  here::here("Outputs/Tables/HVarPart/figure2_region_values.csv")
)
readr::write_csv(
  figure2_balance$record_values,
  here::here(
    "Outputs/Tables/HVarPart/figure2_balance_record_values.csv"
  )
)
readr::write_csv(
  figure2_balance$climatezone_values,
  here::here(
    "Outputs/Tables/HVarPart/figure2_balance_climatezone_values.csv"
  )
)
readr::write_csv(
  figure2_balance$region_values,
  here::here(
    "Outputs/Tables/HVarPart/figure2_balance_region_values.csv"
  )
)
