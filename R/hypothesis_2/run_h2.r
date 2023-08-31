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
# 1. run target pipeline A -----
#----------------------------------------------------------#

# This pipeline should prepare the data

# check the expected targets
data_targest_h2a_manifest <-
  targets::tar_manifest(
    script = here::here("R/hypothesis_2/h2_target_pipeline_a.R")
  )

View(data_targest_h2a_manifest)

targets::tar_visnetwork(
  script = here::here("R/hypothesis_2/h2_target_pipeline_a.R"),
  store = paste0(
    data_storage_path,
    "_targets_h2"
  ),
  targets_only = TRUE
)

# run the target pipeline
targets::tar_make(
  script = here::here("R/hypothesis_2/h2_target_pipeline_a.R"),
  store = paste0(
    data_storage_path,
    "_targets_h2"
  )
)

#----------------------------------------------------------#
# 2. Prepare models to be run on SuperComputer -----
#----------------------------------------------------------#

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

# Prepare jobs for each target
purrr::walk2(
  .progress = TRUE,
  .x = data_targest_h2a_names$name,
  .y = data_targest_h2a_names$name_simple,
  .f = ~ {
    # make a folder
    suppressWarnings(
      try(
        dir.create(
          here::here(
            "jobs", .y
          ),
          recursive = TRUE
        ),
        silent = TRUE
      )
    )

    # targets cannot evaluate the name of a traget programatically, this is a
    # workaround
    eval(
      parse(
        text = paste0(
          "data_target <- targets::tar_read(",
          "name = ", .x, ")"
        )
      )
    )

    # save as rds
    readr::write_rds(
      data_target,
      here::here(
        "jobs", .y, "/data_to_fit.rds"
      )
    )

    # make job
    job_template <-
      readLines(
        here::here(
          "jobs/job_template.sh"
        )
      )

    job_custom <-
      stringr::str_replace_all(
        job_template,
        "--NAME--",
        .y
      )

    writeLines(
      job_custom,
      here::here(
        "jobs", .y, "/job.sh"
      )
    )
  }
)

paste0(
  "sbatch LA/",
  data_targest_h2a_names$name_simple,
  "/job.sh"
) %>%
  paste(., collapse = ";") %>%
  writeLines(
    .,
    here::here(
      "jobs/sbatch_all.txt"
    )
  )

#----------------------------------------------------------#
# 3. run target pipeline A -----
#----------------------------------------------------------#

# This pipeline should run the HVarPart

targets::tar_manifest(
  script = here::here("R/hypothesis_2/h2_target_pipeline_b.R")
) %>%
  View(data_targest_h2b_manifest)

targets::tar_visnetwork(
  script = here::here("R/hypothesis_2/h2_target_pipeline_b.R"),
  store = paste0(
    data_storage_path,
    "_targets_h2"
  ),
  targets_only = TRUE
)

# run the target pipeline
targets::tar_make(
  script = here::here("R/hypothesis_2/h2_target_pipeline_b.R"),
  store = paste0(
    data_storage_path,
    "_targets_h2"
  )
)
