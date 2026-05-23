#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                        Config File
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#
# Configuration script with the variables that should be consistent throughout
#   the whole repo. It loads packages, defines important variables,
#   authorises the user, and saves config file.

# Set the current environment1
current_env <- environment()

# set seed
set_seed <- 1234

#----------------------------------------------------------#
# 1. Load packages -----
#----------------------------------------------------------#

if (
  isFALSE(
    exists("already_synch", envir = current_env)
  )
) {
  already_synch <- FALSE
}

# Synchronise the package versions
if (isFALSE(already_synch)) {
  tryCatch(
    renv::restore(
      lockfile = here::here("renv", "library_list.lock")
    ),
    error = function(err) {
      warning(
        paste(
          "renv::restore() failed; continuing with current environment:",
          conditionMessage(err)
        ),
        call. = FALSE
      )
    }
  )

  already_synch <- TRUE
}

# Save snapshot of package versions
# renv::snapshot(lockfile = here::here("renv", "library_list.lock"))  # do only for update

# Define packages
package_list <-
  c(
    "assertthat",
    "brms",
    "colorspace",
    "furrr",
    "future",
    "geosphere",
    "ggeffects",
    "ggpubr",
    "here",
    "httpgd",
    "insight",
    "janitor",
    "jsonlite",
    "knitr",
    "languageserver",
    "parallelly",
    "rcarbon",
    "rdacca.hp",
    "dplyr",
    "magrittr",
    "purrr",
    "readr",
    "REcopol",
    "renv",
    "remotes",
    "rlang",
    "RRatepol",
    "RUtilpol",
    "stringr",
    "targets",
    "terra",
    "tibble",
    "tidyr",
    "stringr",
    "tidyverse",
    "usethis",
    "utils",
    "vegan",
    "waffle",
    "yaml"
  )

# Attach all packages that are available and warn for missing ones.
load_package_safely <- function(package_name) {
  if (requireNamespace(package_name, quietly = TRUE)) {
    suppressPackageStartupMessages(
      library(package_name, character.only = TRUE)
    )
  } else {
    warning(
      paste0("Package not available in current environment: ", package_name),
      call. = FALSE
    )
  }
}

invisible(
  lapply(package_list, load_package_safely)
)


#----------------------------------------------------------#
# 2. Define space -----
#----------------------------------------------------------#

current_date <- Sys.Date()

# project directory is set up by 'here' package, Adjust if needed
current_dir <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)


#----------------------------------------------------------#
# 3. Load functions -----
#----------------------------------------------------------#

# get vector of general functions and source them
invisible(
  lapply(
    list.files(
      path = here::here("R", "functions"),
      pattern = "*.R",
      recursive = TRUE,
      full.names = TRUE
    ),
    source
  )
)


#----------------------------------------------------------#
# 4. Define directory for external storage -----
#----------------------------------------------------------#

# !!!  IMPORTANT  !!!

# This solution was created for members of HOPE team to store data
#   in a common directory

# If you want to run this project make sure to coppy all data into
#     the `Data` folder in the root directory of the project

if (
  file.exists(
    here::here("secrets.yaml")
  )
) {
  data_storage_path <-
    yaml::read_yaml(
      here::here("secrets.yaml")
    ) |>
    purrr::pluck("Data")
} else {
  data_storage_path <-
    "Data"
}

if (dir.exists(data_storage_path)) {
  check_storage_folders(data_storage_path)
} else {
  warning(
    paste0(
      "Configured data storage path does not exist: ",
      data_storage_path,
      ". Continuing without external storage checks."
    ),
    call. = FALSE
  )
}


#----------------------------------------------------------#
# 5. Define variables -----
#----------------------------------------------------------#

min_age <- 0
max_age <- 12e3
timestep <- 500

# - age table for dummy data
data_dummy_time <- data.frame(
  age = seq(
    from = min_age, # [config]
    to = max_age, # [config]
    by = timestep # [config]
  )
)

vec_regions <-
  setNames(
    c(
      "North America",
      "Latin America",
      "Europe",
      "Asia",
      "Oceania"
    ),
    c(
      "North America",
      "Latin America",
      "Europe",
      "Asia",
      "Oceania"
    )
  )


# regional limits
data_regional_limits <-
  data.frame(
    region = vec_regions,
    xmin = c(-170, -103, -10, 30, 110),
    xmax = c(-50, -23, 40, 175.5, 154),
    ymin = c(15, -56, 35, 0, -45),
    ymax = c(80, 34, 70, 80, -3),
    stringsAsFactors = FALSE
  )

vec_climate_5 <-
  c(
    "Polar",
    "Cold",
    "Temperate",
    "Tropical",
    "Arid"
  )

short_name_climatezone <- c(
  "Polar" = "Polar",
  "Cold_Without_dry_season_Cold_Summer" = "Cold_Cold_Summer",
  "Cold_Without_dry_season_Warm_Summer" = "Cold_Warm_Summer",
  "Cold_Without_dry_season_Hot_Summer" = "Cold_Hot_Summer",
  "Cold_Dry_Winter" = "Cold_Dry_Winter",
  "Cold_Dry_Summer" = "Cold_Dry_Summer",
  "Temperate_Without_dry_season" = "Temperate",
  "Temperate_Dry_Winter" = "Temperate_Dry_Winter",
  "Temperate_Dry_Summer" = "Temperate_Dry_Summer",
  "Tropical" = "Tropical",
  "Arid" = "Arid"
)

data_climate_zones <-
  data.frame(
    climatezone = factor(
      c(
        "Polar",
        "Cold_Cold_Summer",
        "Cold_Warm_Summer",
        "Cold_Hot_Summer",
        "Cold_Dry_Winter",
        "Cold_Dry_Summer",
        "Temperate",
        "Temperate_Dry_Winter",
        "Temperate_Dry_Summer",
        "Tropical",
        "Arid"
      ),
      levels = c(
        "Polar",
        "Cold_Cold_Summer",
        "Cold_Warm_Summer",
        "Cold_Hot_Summer",
        "Cold_Dry_Winter",
        "Cold_Dry_Summer",
        "Temperate",
        "Temperate_Dry_Winter",
        "Temperate_Dry_Summer",
        "Tropical",
        "Arid"
      )
    ),
    climatezone_label = c(
      "Polar",
      "Cold - Cold Summer",
      "Cold - Warm Summer",
      "Cold - Hot Summer",
      "Cold - Dry Winter",
      "Cold - Dry Summer",
      "Temperate",
      "Temperate - Dry Winter",
      "Temperate - Dry Summer",
      "Tropical",
      "Arid"
    ),
    stringsAsFactors = FALSE
  )

min_n_records_per_climate_zone <- 5

min_number_of_rc_dates <- 50

#----------------------------------------------------------#
# 6. Graphical options -----
#----------------------------------------------------------#


# set ggplot output
ggplot2::theme_set(
  ggplot2::theme_classic()
)

# define general
text_size <- 10
line_size <- 0.1
point_size <- 1

# define output sizes
image_width_vec <-
  setNames(
    c(
      90,
      180,
      270
    ),
    c("1col", "2col", "3col")
  )
image_units <- "mm"

# region labeller
region_labeller <- c(
  `North America` = "North America",
  `Latin America` = "Central & South America",
  `Europe` = "Europe",
  `Asia` = "Asia",
  `Oceania` = "Oceania"
)


# Define colour palette

common_gray <- "#636363"

palette_ecozones <-
  c(
    "Polar" = "#907A8E",
    "Cold - Cold Summer" = "#8C4418",
    "Cold - Warm Summer" = "#DC702E",
    "Cold - Hot Summer" = "#AA6133",
    "Cold - Dry Winter" = "#CB8152",
    "Cold - Dry Summer" = "#E59463",
    "Temperate" = "#371E71",
    "Temperate - Dry Winter" = "#562FB1",
    "Temperate - Dry Summer" = "#9A7EDD",
    "Arid" = "#DDDF78",
    "Tropical" = "#D68FD6"
  )

# Predictors
palette_predictors <- c(
  human = "#c99b38", # v#ffa600"
  climate = "#1f6f6f" # v#d74e92"
)

paletete_age <-
  c(
    "young" = "#66C3FF",
    "old" = "#1337a3"
  )
