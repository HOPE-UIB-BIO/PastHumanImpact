#----------------------------------------------------------#
#
#
#                     Project name
#
#               Predict predictor general trends
#
#
#                    O. Mottl, V. Felde
#                         2023
#
#----------------------------------------------------------#
# Load hGAM models and predict with general petterns


#----------------------------------------------------------#
# 0. Set up -----
#----------------------------------------------------------#

library(tidyverse)
library(insight)
library(targets)

# set configuration for _target storage
targets::tar_config_set(
  store = here::here("_targets1")
)

invisible(
  lapply(
    list.files(
      path = "R/functions",
      pattern = "*.R",
      recursive = TRUE,
      full.names = TRUE
    ),
    source
  )
)

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_mods <-
  list.files(
    here::here("Data/Proccesed/Models/hGAM/Predictors"),
    full.names = FALSE
  ) %>%
  RUtilpol::get_clean_name() %>%
  rlang::set_names() %>%
  purrr::map(
    .progress = "Loading all data",
    .f = ~ RUtilpol::get_latest_file(
      file_name = .x,
      dir = here::here("Data/Proccesed/Models/hGAM/Predictors"),
      verbose = FALSE
    )
  ) %>%
  purrr::discard(
    .p = is.na(.)
  ) %>%
  tibble::tibble(
    full_name = names(.),
    mod = .
  )

data_dummy_time <-
  targets::tar_read("data_dummy_time")


#----------------------------------------------------------#
# 2. Predict models -----
#----------------------------------------------------------#

data_mods_with_name <-
  data_mods %>%
  dplyr::mutate(
    var_name = stringr::str_extract(full_name, "^[^_]*"),
    group = stringr::str_extract(full_name, "(?<=_).*")
  )

list_mods_pred <-
  purrr::pmap(
    .progress = TRUE,
    .l = list(
      data_mods_with_name$mod,
      data_mods_with_name$var_name
    ),
    .f = ~ predict_hgam(
      data_model = ..1,
      var_name = ..2,
      data_dummy = data_dummy_time,
      time_step = diff(data_dummy_time$age) %>%
        unique()
    )
  ) %>%
  rlang::set_names(
    nm = data_mods_with_name$full_name
  )

data_mods_pred <-
  dplyr::bind_rows(
    .id = "full_name",
    list_mods_pred
  ) %>%
  dplyr::group_by(full_name) %>%
  tidyr::nest(
    data_pred = -full_name
  ) %>%
  dplyr::ungroup()

data_pred <-
  data_mods_with_name %>%
  dplyr::inner_join(
    data_mods_pred,
    by = "full_name"
  ) %>%
  dplyr::mutate(
    data_raw = purrr::map(
      .x = mod,
      .f = ~ insight::get_data(.x) %>%
        tibble::as_tibble() %>%
        rlang::set_names(
          nm = c("value", "age", "dataset_id", "(weights)", "weights"),
        )
    ),
    data_pred_simple = purrr::map(
      .x = data_pred,
      .f = ~ .x %>%
        dplyr::select(
          dplyr::any_of(
            c(
              "age",
              "lwr",
              "upr",
              "prec",
              "temp",
              "spd"
            )
          )
        ) %>%
        tidyr::pivot_longer(
          cols = -c(age, lwr, upr)
        ) %>%
        tidyr::drop_na(value)
    )
  )

data_with_plots <-
  data_pred %>%
  dplyr::mutate(
    fig = purrr::pmap(
      .l = list(
        data_raw,
        data_pred_simple,
        var_name,
        full_name
      ),
      .f = ~ plot_hgam_result(
        data_source_raw = ..1,
        data_source_pred = ..2,
        y_var_name = ..3,
        title = ..4
      )
    )
  )


ggpubr::ggarrange(
  plotlist = data_with_plots$fig
)
