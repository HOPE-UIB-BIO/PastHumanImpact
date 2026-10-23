#----------------------------------------------------------#
# Run SPD plus KK10 H1 HVarPart using the shared human-proxy graph
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

# 1. Supply the explicit profile to the shared target script -----

# Project configuration sources functions into the global environment. Use that
# environment explicitly so targets fingerprints shared helper changes too.
human_proxy_hvarpart_profile <- "spd_kk10"

# 2. Run in a dedicated store, retaining canonical and joint results -----

tryCatch(
  targets::tar_make(
    script = here::here(
      "R", "analyses", "91_sensitivity_analyses",
      "joint_human_proxy_hvarpart", "pipeline.R"
    ),
    store = resolve_pipeline_store_path(
      data_storage_path = data_storage_path,
      store_relative_path = "sensitivity_analyses/spd_kk10_hvarpart"
    ),
    envir = globalenv(),
    callr_function = NULL
  ),
  finally = rm("human_proxy_hvarpart_profile", envir = globalenv())
)
