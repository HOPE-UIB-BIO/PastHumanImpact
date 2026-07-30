#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#                 Hypothesis I: Figure 3
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

#----------------------------------------------------------#
# 1. Load and extract fitted results -----
#----------------------------------------------------------#
store_h1 <- paste0(data_storage_path, "Targets_data/analyses_h1")
output_temporal_spd <- targets::tar_read(
  "output_temporal_spd",
  store = store_h1
)
output_temporal_events <- targets::tar_read(
  "output_temporal_events",
  store = store_h1
)

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
) |>
  dplyr::filter(
    dplyr::between(.data[["age"]], 0, 8500),
    .data[["analysis"]] != "temporal_spd" | .data[["age"]] >= 2000
  )

#----------------------------------------------------------#
# 2. Summarise display profiles -----
#----------------------------------------------------------#
data_temporal_zero_truncated <- summarise_hvarpart_importance(
  data_importance = data_importance,
  group_vars = c("analysis", "region", "age"),
  profile = "zero_truncated"
)
data_temporal_signed <- summarise_hvarpart_importance(
  data_importance = data_importance,
  group_vars = c("analysis", "region", "age"),
  profile = "signed"
)
data_temporal_balance <- get_hvarpart_importance_balance(
  data_summary = data_temporal_zero_truncated,
  group_vars = c("analysis", "region", "age")
)

#----------------------------------------------------------#
# 3. Build main and supplementary figures -----
#----------------------------------------------------------#
figure3_main <- plot_hvarpart_temporal_balance(data_temporal_balance)
figure3_signed <- plot_hvarpart_temporal_signed(data_temporal_signed)

#----------------------------------------------------------#
# 4. Save figures and source tables -----
#----------------------------------------------------------#
dir.create(
  here::here("Outputs/Figures/Extended_data_figures/HVarPart"),
  recursive = TRUE,
  showWarnings = FALSE
)
save_figure3 <- function(extension) {
  ggplot2::ggsave(
    paste0(
      here::here("Outputs/Figures/Figure3_h1_temporal"),
      ".",
      extension
    ),
    plot = figure3_main,
    width = image_width_vec[["2col"]],
    height = 165,
    units = image_units,
    bg = "white"
  )
  ggplot2::ggsave(
    paste0(
      here::here(
        paste0(
          "Outputs/Figures/Extended_data_figures/HVarPart/",
          "Figure3_h1_temporal_signed_full_range"
        )
      ),
      ".",
      extension
    ),
    plot = figure3_signed,
    width = image_width_vec[["2col"]],
    height = 165,
    units = image_units,
    bg = "white"
  )
}
purrr::walk(c("png", "pdf"), save_figure3)

readr::write_csv(
  data_temporal_zero_truncated,
  here::here("Outputs/Tables/summary_temporal_zero_truncated.csv")
)
readr::write_csv(
  data_temporal_signed,
  here::here("Outputs/Tables/summary_temporal_signed.csv")
)
readr::write_csv(
  data_temporal_balance,
  here::here("Outputs/Tables/summary_temporal_balance.csv")
)
