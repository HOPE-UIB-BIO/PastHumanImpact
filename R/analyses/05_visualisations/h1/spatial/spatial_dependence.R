#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#          Spatial-dependence diagnostic figures
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
# 1. Load current reciprocal-control exports -----
#----------------------------------------------------------#
path_table_dir <-
  here::here(
    "Outputs",
    "Tables",
    "Diagnostics",
    "Spatiotemporal_dependence"
  )

data_moran <-
  readr::read_csv(
    file.path(
      path_table_dir,
      stringr::str_c(
        "spd__human_climate_balance__moran_diagnostics__",
        "time_and_space_control.csv"
      )
    ),
    show_col_types = FALSE
  )

data_sensitivity <-
  readr::read_csv(
    file.path(
      path_table_dir,
      stringr::str_c(
        "spd__human_climate_balance__sensitivity__",
        "time_and_space_control.csv"
      )
    ),
    show_col_types = FALSE
  )

data_estimates <-
  readr::read_csv(
    file.path(
      path_table_dir,
      stringr::str_c(
        "spd__human_climate_balance__estimates__",
        "time_and_space_control.csv"
      )
    ),
    show_col_types = FALSE
  )

#----------------------------------------------------------#
# 2. Build profile-specific diagnostics -----
#----------------------------------------------------------#
vec_profiles <-
  c("signed", "zero_truncated")

list_moran_plots <-
  vec_profiles |>
  purrr::map(
    .f = ~ plot_spatial_moran_profile(
      data_moran = data_moran,
      profile = .x
    )
  ) |>
  rlang::set_names(vec_profiles)

list_thinning_plots <-
  vec_profiles |>
  purrr::map(
    .f = ~ plot_spatial_thinning_profile(
      data_sensitivity = data_sensitivity,
      data_estimates = data_estimates,
      profile = .x
    )
  ) |>
  rlang::set_names(vec_profiles)

vec_profile_names <-
  c(
    signed = "untruncated_hierarchical_contribution_difference",
    zero_truncated = "zero_truncated_hierarchical_composition"
  )

#----------------------------------------------------------#
# 3. Save figures -----
#----------------------------------------------------------#
path_figure_dir <-
  here::here(
    "Outputs",
    "Figures",
    "H1",
    "Spatial",
    "SPD",
    "Spatial_dependence"
  )

dir.create(
  path_figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ {
    extension <- .x

    purrr::iwalk(
      .x = list_moran_plots,
      .f = ~ ggplot2::ggsave(
        filename = file.path(
          path_figure_dir,
          stringr::str_c(
            "spd__human_climate_balance__moran_i__",
            vec_profile_names[[.y]],
            "__time_and_space_control.",
            extension
          )
        ),
        plot = .x,
        width = image_width_vec[["2col"]],
        height = 135,
        units = image_units,
        bg = "white"
      )
    )

    purrr::iwalk(
      .x = list_thinning_plots,
      .f = ~ ggplot2::ggsave(
        filename = file.path(
          path_figure_dir,
          stringr::str_c(
            "spd__human_climate_balance__spatial_thinning__",
            vec_profile_names[[.y]],
            "__time_and_space_control.",
            extension
          )
        ),
        plot = .x,
        width = image_width_vec[["2col"]],
        height = 105,
        units = image_units,
        bg = "white"
      )
    )
  }
)
