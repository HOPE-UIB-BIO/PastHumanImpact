#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Hypothesis II
#
#
#                   O. Mottl, V. Felde
#                         2023
#
#----------------------------------------------------------#



#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

targets::tar_config_set(
  store = paste0(
    data_storage_path,
    "_targets_h2"
  ),
  project = "project_h2"
)

Sys.setenv(TAR_PROJECT = "project_h2")


#----------------------------------------------------------#
# 1. Prepare data -----
#----------------------------------------------------------#

# check the expected targets
data_targest_h2a_manifest <-
  targets::tar_manifest(
    script = here::here("R/hypothesis_2/h2_target_pipeline_a.R")
  )

data_targest_h2a_names <-
  data_targest_h2a_manifest %>%
  dplyr::filter(
    stringr::str_detect(name, "data_to_fit")
  ) %>%
  dplyr::distinct(name) %>%
  dplyr::mutate(
    name_data_fixed = stringr::str_replace_all(name, "\\.", "_"),
    name_simple = stringr::str_replace(name_data_fixed, "data_to_fit_mod_", ""),
  )

.x <- data_targest_h2a_names$name[[1]]
.y <- data_targest_h2a_names$name_simple[[1]]

#----------------------------------------------------------#
# 2. Run models locally -----
#----------------------------------------------------------#

purrr::walk2(
  .progress = TRUE,
  .x = data_targest_h2a_names$name,
  .y = data_targest_h2a_names$name_simple,
  .f = ~ {
    message(.y)

    if (
      file.exists(
        here::here(
          "jobs", .y, "mod.rds"
        )
      )
    ) {
      message(" - skip")
      return()
    }

    # targets cannot evaluate the name of a traget programatically, this is a
    # workaround
    eval(
      parse(
        text = paste0(
          "data_list <- targets::tar_read(",
          "name = ", .x, ")"
        )
      )
    )

    sel_data <-
      data_list %>%
      purrr::chuck("sel_data")

    if (
      nrow(sel_data) > 0
    ) {
      n_recors <-
        sel_data %>%
        purrr::chuck("dataset_id") %>%
        unique() %>%
        length()

      message(n_recors)

      use_parallel <-
        parallel::detectCores(logical = FALSE) < n_recors

      # Fit GAM model
      data_mod <-
        REcopol::fit_hgam(
          data_source = sel_data,
          x_var = data_list %>%
            purrr::chuck("x_var"),
          y_var = data_list %>%
            purrr::chuck("y_var"),
          group_var = data_list %>%
            purrr::chuck("group_var"),
          weights_var = data_list %>%
            purrr::chuck("weights_var"),
          smooth_basis = data_list %>%
            purrr::chuck("smooth_basis"),
          error_family = data_list %>%
            purrr::chuck("error_family"),
          sel_k = data_list %>%
            purrr::chuck("sel_k"),
          common_trend = TRUE,
          use_parallel = use_parallel,
          max_iterations = 200,
          verbose = TRUE
        )

      readr::write_rds(
        data_mod,
        file = here::here(
          "jobs", .y, "mod.rds"
        )
      )

      message(" - done")
    }
  }
)
