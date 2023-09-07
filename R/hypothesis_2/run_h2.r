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
            data_storage_path, "h2_predictor_jobs", .y
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
        data_storage_path, "h2_predictor_jobs", .y, "/data_to_fit.rds"
      )
    )

    # make job
    job_slurm_template <-
      readLines(
        here::here(
          data_storage_path,
          "h2_predictor_jobs/job_template_slurm.sh"
        )
      )

    job_slurm_custom <-
      stringr::str_replace_all(
        job_slurm_template,
        "--NAME--",
        .y
      ) %>%
      stringr::str_replace_all(
        "--TIME--",
        "24"
      )

    writeLines(
      job_slurm_custom,
      here::here(
        data_storage_path, "h2_predictor_jobs", .y, "/job_slurm.sh"
      )
    )

    job_torque_template <-
      readLines(
        here::here(
          data_storage_path,
          "h2_predictor_jobs/job_template_torque.sh"
        )
      )

    job_torque_custom <-
      stringr::str_replace_all(
        job_torque_template,
        "--NAME--",
        .y
      ) %>%
      stringr::str_replace_all(
        "--TIME--",
        "100"
      )

    writeLines(
      job_torque_custom,
      here::here(
        data_storage_path, "h2_predictor_jobs", .y, "/job_torque.sh"
      )
    )
  }
)

paste0(
  "sbatch LA/",
  data_targest_h2a_names$name_simple,
  "/job_slurm.sh"
) %>%
  paste(., collapse = ";") %>%
  writeLines(
    .,
    here::here(
      data_storage_path,
      "h2_predictor_jobs/sbatch_all.txt"
    )
  )

paste0(
  "qsub LA/",
  data_targest_h2a_names$name_simple,
  "/job_torque.sh"
) %>%
  paste(., collapse = ";") %>%
  writeLines(
    .,
    here::here(
      data_storage_path,
      "h2_predictor_jobs/qsub_all.txt"
    )
  )

#----------------------------------------------------------#
# 3. run target pipeline A -----
#----------------------------------------------------------#

# This pipeline should run the HVarPart

targets::tar_manifest(
  script = here::here("R/hypothesis_2/h2_target_pipeline_b.R")
) %>%
  View()

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
