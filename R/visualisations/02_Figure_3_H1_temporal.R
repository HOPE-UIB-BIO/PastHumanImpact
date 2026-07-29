library(here)
source(here::here("R/00_Config_file.R"))

store_h1 <- paste0(data_storage_path, "Targets_data/analyses_h1")
output_temporal_spd <- targets::tar_read("output_temporal_spd", store = store_h1)
output_temporal_events <- targets::tar_read("output_temporal_events", store = store_h1)

data_importance <- dplyr::bind_rows(
  get_hvarpart_importance(
    output_temporal_spd |>
      dplyr::mutate(analysis = "temporal_spd"),
    id_cols = c("analysis", "region", "age")
  ),
  get_hvarpart_importance(
    output_temporal_events |>
      dplyr::mutate(analysis = "temporal_events"),
    id_cols = c("analysis", "region", "age")
  )
)

data_temporal_signed <- summarise_hvarpart_importance(
  data_importance = data_importance,
  group_vars = c("analysis", "region", "age"),
  profile = "signed"
)

figure3 <- plot_hvarpart_temporal_signed(data_temporal_signed)

purrr::walk(c("png", "pdf"), function(extension) {
  ggplot2::ggsave(
    paste0(here::here("Outputs/Figures/Figure3_h1_temporal"), ".", extension),
    plot = figure3,
    width = image_width_vec[["1col"]],
    height = 130,
    units = image_units,
    bg = "white"
  )
})

readr::write_csv(
  data_temporal_signed,
  here::here("Outputs/Tables/summary_temporal_signed.csv")
)
