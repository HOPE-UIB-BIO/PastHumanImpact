# Corrected recreation of Figure 2. A complete redesign is intentionally
# deferred until this recreation has been reviewed.
library(here)
source(here::here("R/00_Config_file.R"))
source(here::here("R/main_analysis/02_meta_data.R"))

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

figure2_recreation <- plot_hvarpart_spatial_recreation(
  data_importance = data_importance,
  data_meta = data_meta,
  data_geo_koppen = data_geo_koppen
)

dir.create(here::here("Outputs/Tables/HVarPart"), recursive = TRUE, showWarnings = FALSE)
dir.create(
  here::here("Outputs/Figures/Extended_data_figures/HVarPart"),
  recursive = TRUE,
  showWarnings = FALSE
)

purrr::walk(c("png", "pdf"), function(extension) {
  ggplot2::ggsave(
    paste0(here::here("Outputs/Figures/Figure2_h1_spatial"), ".", extension),
    plot = figure2_recreation$central_plot,
    width = image_width_vec[["3col"]],
    height = 170,
    units = image_units,
    bg = "white"
  )
  ggplot2::ggsave(
    paste0(
      here::here(
        "Outputs/Figures/Extended_data_figures/HVarPart/Figure2_h1_spatial_full_range"
      ),
      ".",
      extension
    ),
    plot = figure2_recreation$full_range_plot,
    width = image_width_vec[["3col"]],
    height = 170,
    units = image_units,
    bg = "white"
  )
})

readr::write_csv(
  figure2_recreation$record_values,
  here::here("Outputs/Tables/HVarPart/figure2_record_values.csv")
)
readr::write_csv(
  figure2_recreation$summary_values,
  here::here("Outputs/Tables/HVarPart/figure2_pooled_values.csv")
)
readr::write_csv(
  figure2_recreation$tail_counts,
  here::here("Outputs/Tables/HVarPart/figure2_display_tail_counts.csv")
)
