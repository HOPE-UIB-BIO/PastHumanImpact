#----------------------------------------------------------#
# Collinearity-managed human-proxy dataset temporal examples
#----------------------------------------------------------#
# Reuses the established trajectory preparation and layout, then replaces the
# HVarPart panel with the locally selected human and climate blocks.

library(here)
source(here::here("R/00_Config_file.R"))

source(here::here(
  "R", "analyses", "91_sensitivity_analyses", "joint_human_proxy_hvarpart",
  "example_datasets.R"
))

path_colmanaged_store <- resolve_pipeline_store_path(
  data_storage_path,
  "sensitivity_analyses/human_proxy_hvarpart_collinearity_managed"
)
path_colmanaged_runner <- here::here(
  "R", "analyses", "91_sensitivity_analyses",
  "human_proxy_hvarpart_collinearity_managed", "00_run.R"
)
path_colmanaged_examples <- here::here(
  "Outputs", "Figures", "H1", "Sensitivity",
  "Human_proxy_hvarpart_collinearity_managed", "Dataset_examples"
)
run_directory_setup(path_colmanaged_examples)

data_local_fits <- load_target_store_value(
  store = path_colmanaged_store,
  target_name = "output_colmanaged_temporal_fits",
  runner = path_colmanaged_runner
) |>
  dplyr::filter(
    .data[["dataset_id"]] %in% vec_example_dataset_ids,
    .data[["model_id"]] == "joint_filtered"
  )
assertthat::assert_that(
  nrow(data_local_fits) == length(vec_example_dataset_ids),
  all(purrr::map_chr(data_local_fits$result, "status") %in%
    c("estimated", "estimated_residual_temporal_dependence")),
  msg = "Both example datasets require estimable locally selected models."
)

output_local_human_climate <- data_local_fits |>
  dplyr::transmute(
    dataset_id = .data[["dataset_id"]],
    varhp = purrr::map(.data[["result"]], "human_climate_only_hvarpart")
  )
data_local_importance <- compute_hvarpart_importance(
  data_source = output_local_human_climate,
  id_cols = "dataset_id"
)
data_local_selection <- data_local_fits |>
  dplyr::transmute(
    dataset_id = .data[["dataset_id"]],
    selected_human = purrr::map_chr(
      .data[["predictor_vars"]],
      ~ stringr::str_c(.x$human, collapse = "; ")
    ),
    selected_climate = purrr::map_chr(
      .data[["predictor_vars"]],
      ~ stringr::str_c(.x$climate, collapse = "; ")
    ),
    n_selected_human = purrr::map_int(.data[["predictor_vars"]], ~ length(.x$human))
  )

readr::write_csv(
  data_selected_examples |>
    dplyr::left_join(data_local_importance, by = "dataset_id") |>
    dplyr::left_join(data_local_selection, by = "dataset_id") |>
    dplyr::mutate(
      joint_human_dominated_label_allowed =
        .data[["n_selected_human"]] >= 2L
    ),
  file.path(
    path_colmanaged_examples,
    "collinearity_managed__dataset_example_selection.csv"
  )
)

data_local_figure_inputs <- data_selected_examples |>
  dplyr::transmute(
    dataset_id = .data[["dataset_id"]],
    example_type = .data[["example_type"]],
    data_raw = unname(list_raw_by_dataset[vec_selected_ids]),
    data_observed = unname(list_observed_by_dataset[vec_selected_ids]),
    data_predictions = unname(list_predictions_by_dataset[vec_selected_ids]),
    data_metadata = unname(list_metadata_by_dataset[vec_selected_ids])
  ) |>
  dplyr::left_join(data_local_selection, by = "dataset_id") |>
  dplyr::mutate(
    plot = purrr::pmap(
      list(data_raw, data_observed, data_predictions, data_metadata),
      ~ plot_hvarpart_dataset_temporal_example(
        data_raw = ..1, data_observed = ..2, data_predictions = ..3,
        data_metadata = ..4, data_importance = data_local_importance
      )
    ),
    panel_label = stringr::str_glue(
      "{c('A', 'B')} {ifelse(example_type == 'climate', 'Climate example', 'Human-proxy example')}\n",
      "Selected human: {selected_human}; climate: {selected_climate}"
    ),
    plot_labelled = purrr::map2(
      .data[["plot"]], .data[["panel_label"]],
      ~ cowplot::ggdraw() +
        cowplot::draw_plot(.x, x = 0, y = 0, width = 1, height = 0.94) +
        cowplot::draw_label(
          .y, x = 0, y = 1, hjust = 0, vjust = 1,
          fontface = "bold", size = 9
        )
    )
  )
plot_local_examples <- cowplot::plot_grid(
  plotlist = data_local_figure_inputs$plot_labelled,
  ncol = 1, align = "v"
)
purrr::walk(c("png", "pdf"), ~ ggplot2::ggsave(
  filename = file.path(
    path_colmanaged_examples,
    stringr::str_glue(
      "collinearity_managed__dataset_temporal_examples.{.x}"
    )
  ),
  plot = plot_local_examples,
  width = 390, height = 300, units = "mm", dpi = 300, bg = "white"
))
