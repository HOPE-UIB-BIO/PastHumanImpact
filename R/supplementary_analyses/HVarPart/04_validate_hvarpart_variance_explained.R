#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#      Validate HVarPart variation and correlations
#
#                         2026
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)
source(
  here::here("R/00_Config_file.R")
)

path_output_tables <-
  here::here("Outputs/Tables/HVarPart")

path_output_figures <-
  here::here(
    "Outputs/Figures/Extended_data_figures/HVarPart"
  )

tolerance <- 0.001

#----------------------------------------------------------#
# 1. Load exported data -----
#----------------------------------------------------------#

data_decomposition <-
  readr::read_csv(
    file.path(
      path_output_tables,
      "hvarpart_variance_decomposition_models.csv"
    ),
    show_col_types = FALSE
  )

data_records <-
  readr::read_csv(
    file.path(
      path_output_tables,
      "HVarPart_spatial_adjr2_distribution_records.csv"
    ),
    show_col_types = FALSE
  )

data_climatezone <-
  readr::read_csv(
    file.path(
      path_output_tables,
      "HVarPart_spatial_adjr2_distribution_climatezones.csv"
    ),
    show_col_types = FALSE
  )

data_continent <-
  readr::read_csv(
    file.path(
      path_output_tables,
      "HVarPart_spatial_adjr2_distribution_continents.csv"
    ),
    show_col_types = FALSE
  )

data_correlation <-
  readr::read_csv(
    file.path(
      path_output_tables,
      "HVarPart_adjr2_human_importance_model_values.csv"
    ),
    show_col_types = FALSE
  )

table_overall <-
  readr::read_csv(
    file.path(
      path_output_tables,
      "HVarPart_adjr2_human_importance_overall_statistics.csv"
    ),
    show_col_types = FALSE
  )

table_grid <-
  readr::read_csv(
    file.path(
      path_output_tables,
      "HVarPart_adjr2_human_importance_grid_statistics.csv"
    ),
    show_col_types = FALSE
  )

#----------------------------------------------------------#
# 2. Validate variance accounting -----
#----------------------------------------------------------#

data_available <-
  data_decomposition |>
  dplyr::filter(.data[["has_finite_decomposition"]])
stopifnot(
  all(abs(data_available[["accounting_residual"]]) <= tolerance),
  all(abs(data_available[["full_accounting_residual"]]) <= tolerance),
  all(abs(data_available[["varpart_total_residual"]]) <= tolerance),
  all(
    abs(data_available[["bounded_accounting_residual"]]) <=
      .Machine[["double.eps"]] ^ 0.5
  )
)

#----------------------------------------------------------#
# 3. Validate adjusted R-squared distributions -----
#----------------------------------------------------------#

summarise_distribution_independent <- function(
  data,
  group_vars
) {
  data_summary <-
    data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      n_models = as.double(dplyr::n()),
      mean_adjusted_r_squared = mean(.data[["adjusted_r_squared"]]),
      sd_adjusted_r_squared = stats::sd(.data[["adjusted_r_squared"]]),
      min_adjusted_r_squared = min(.data[["adjusted_r_squared"]]),
      q1_adjusted_r_squared = stats::quantile(
        .data[["adjusted_r_squared"]],
        probs = 0.25,
        names = FALSE
      ),
      median_adjusted_r_squared = stats::median(
        .data[["adjusted_r_squared"]]
      ),
      q3_adjusted_r_squared = stats::quantile(
        .data[["adjusted_r_squared"]],
        probs = 0.75,
        names = FALSE
      ),
      max_adjusted_r_squared = max(.data[["adjusted_r_squared"]]),
      .groups = "drop"
    )

  return(data_summary)
}

recomputed_climatezone <-
  summarise_distribution_independent(
    data = data_records,
    group_vars = c("region", "climatezone", "climatezone_label")
  ) |>
  dplyr::arrange(
    .data[["region"]],
    .data[["climatezone"]]
  )
recomputed_continent <-
  summarise_distribution_independent(
    data = data_records,
    group_vars = "region"
  ) |>
  dplyr::arrange(.data[["region"]])
checked_climatezone <-
  data_climatezone |>
  dplyr::select(dplyr::all_of(names(recomputed_climatezone))) |>
  dplyr::arrange(
    .data[["region"]],
    .data[["climatezone"]]
  )
checked_continent <-
  data_continent |>
  dplyr::select(dplyr::all_of(names(recomputed_continent))) |>
  dplyr::arrange(.data[["region"]])
stopifnot(
  isTRUE(
    all.equal(
      recomputed_climatezone,
      checked_climatezone,
      check.attributes = FALSE
    )
  ),
  isTRUE(
    all.equal(
      recomputed_continent,
      checked_continent,
      check.attributes = FALSE
    )
  ),
  any(data_records[["adjusted_r_squared"]] < 0),
  nrow(data_records) == sum(data_climatezone[["n_models"]])
)

#----------------------------------------------------------#
# 4. Validate signed correlations -----
#----------------------------------------------------------#

recomputed_overall <-
  summarise_hvarpart_correlations(
    data_values = data_correlation,
    importance_column = "human_importance_signed"
  )
recomputed_grid <-
  summarise_hvarpart_correlations(
    data_values = data_correlation,
    group_vars = c("region", "climatezone"),
    importance_column = "human_importance_signed"
  ) |>
  dplyr::arrange(
    .data[["region"]],
    .data[["climatezone"]]
  )
checked_grid <-
  table_grid |>
  add_climatezone_as_factor() |>
  dplyr::select(dplyr::all_of(names(recomputed_grid))) |>
  dplyr::mutate(
    climatezone = as.character(.data[["climatezone"]])
  ) |>
  dplyr::arrange(
    .data[["region"]],
    .data[["climatezone"]]
  )
correlation_columns <-
  c(
    "spearman_rho",
    "pearson_r"
  )
overall_noncorrelation_columns <-
  setdiff(names(recomputed_overall), correlation_columns)
grid_noncorrelation_columns <-
  setdiff(names(recomputed_grid), correlation_columns)
stopifnot(
  nrow(table_overall) == 1L,
  all(
    table_overall[["importance_profile"]] ==
      "human_importance_signed"
  ),
  all(
    table_grid[["importance_profile"]] ==
      "human_importance_signed"
  ),
  isTRUE(
    all.equal(
      recomputed_overall[overall_noncorrelation_columns],
      tibble::as_tibble(table_overall)[overall_noncorrelation_columns],
      check.attributes = FALSE
    )
  ),
  isTRUE(
    all.equal(
      recomputed_grid[grid_noncorrelation_columns],
      tibble::as_tibble(checked_grid)[grid_noncorrelation_columns],
      check.attributes = FALSE
    )
  ),
  identical(
    is.na(recomputed_overall[["spearman_rho"]]),
    is.na(table_overall[["spearman_rho"]])
  ),
  identical(
    is.na(recomputed_grid[["spearman_rho"]]),
    is.na(checked_grid[["spearman_rho"]])
  ),
  max(
    abs(
      recomputed_overall[["spearman_rho"]] -
        table_overall[["spearman_rho"]]
    ),
    na.rm = TRUE
  ) <= tolerance,
  max(
    abs(
      recomputed_grid[["spearman_rho"]] -
        checked_grid[["spearman_rho"]]
    ),
    na.rm = TRUE
  ) <= tolerance,
  max(
    abs(
      recomputed_overall[["pearson_r"]] -
        table_overall[["pearson_r"]]
    ),
    na.rm = TRUE
  ) <= .Machine[["double.eps"]] ^ 0.5,
  max(
    abs(
      recomputed_grid[["pearson_r"]] -
        checked_grid[["pearson_r"]]
    ),
    na.rm = TRUE
  ) <= .Machine[["double.eps"]] ^ 0.5,
  all(
    unique(data_correlation[["climatezone"]]) %in%
      names(palette_ecozones)
  ),
  all(unique(data_correlation[["region"]]) %in% unname(vec_regions)),
  all(
    unique(data_correlation[["climatezone_label"]]) %in%
      get_climatezone_label(data_climate_zones[["climatezone_label"]])
  )
)

#----------------------------------------------------------#
# 5. Validate figure artifacts -----
#----------------------------------------------------------#

required_figures <-
  c(
    "HVarPart_spatial_adjr2_distribution",
    "HVarPart_adjr2_human_importance_overall",
    "HVarPart_adjr2_human_importance_grid"
  )

obsolete_figures <-
  c(
    "HVarPart_spatial_cumulative_adjr2",
    "HVarPart_spatial_cumulative_adjr2_zero_truncated_sensitivity",
    "HVarPart_adjr2_human_importance_overall_signed_sensitivity",
    "HVarPart_adjr2_human_importance_grid_signed_sensitivity"
  )

figure_extensions <-
  c(
    "png",
    "pdf"
  )

required_figure_files <-
  tidyr::crossing(
    figure_name = required_figures,
    extension = figure_extensions
  ) |>
  dplyr::transmute(
    path_file = stringr::str_c(
      .data[["figure_name"]],
      ".",
      .data[["extension"]]
    )
  ) |>
  dplyr::pull(.data[["path_file"]])

obsolete_figure_files <-
  tidyr::crossing(
    figure_name = obsolete_figures,
    extension = figure_extensions
  ) |>
  dplyr::transmute(
    path_file = stringr::str_c(
      .data[["figure_name"]],
      ".",
      .data[["extension"]]
    )
  ) |>
  dplyr::pull(.data[["path_file"]])

obsolete_tables <-
  c(
    "HVarPart_spatial_cumulative_adjr2_records.csv",
    "HVarPart_spatial_cumulative_adjr2_climatezones.csv",
    "HVarPart_spatial_cumulative_adjr2_continents.csv"
  )

stopifnot(
  all(
    file.exists(
      file.path(
        path_output_figures,
        required_figure_files
      )
    )
  ),
  !any(
    file.exists(
      file.path(
        path_output_figures,
        obsolete_figure_files
      )
    )
  ),
  !any(
    file.exists(
      file.path(
        path_output_tables,
        obsolete_tables
      )
    )
  )
)

#----------------------------------------------------------#
# 6. Report validation success -----
#----------------------------------------------------------#

cat(
  "Validated decomposition accounting, adjusted R-squared distributions,",
  "signed correlations, palette/group positions, and figure artifacts.\n"
)
