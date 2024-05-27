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

# Set the current environment
current_env <- environment()

# set seed
set_seed <- 1234
set.seed(set_seed)

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

if (
  isFALSE(already_synch)
) {
  library(here)
  # Synchronise the package versions
  renv::restore(
    lockfile = here::here("renv/library_list.lock")
  )
  already_synch <- TRUE

  # Save snapshot of package versions
  # renv::snapshot(lockfile = here::here("renv/library_list.lock"))  # do only for update
}

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
    "REcopol",
    "renv",
    "remotes",
    "rlang",
    "RRatepol",
    "RUtilpol",
    "targets",
    "terra",
    "tidyverse",
    "usethis",
    "utils",
    "vegan",
    "waffle",
    "yaml"
  )

# Attach all packages
sapply(package_list, library, character.only = TRUE)


#----------------------------------------------------------#
# 2. Define space -----
#----------------------------------------------------------#

current_date <- Sys.Date()

# project directory is set up by 'here' package, Adjust if needed
current_dir <- here::here()


#----------------------------------------------------------#
# 3. Load functions -----
#----------------------------------------------------------#

# get vector of general functions and source them
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
# 4.Define directory for external storage -----
#----------------------------------------------------------#

# !!!  IMPORTANT  !!!

# This solution was created for members of HOPE team to store data
#   in a common directory

# If you want to run this project make sure to coppy all data into
#     the Data folder in the root directory of the project

if (
  file.exists(
    here::here("secrets.yaml")
  )
) {
  data_storage_path <-
    yaml::read_yaml(
      here::here("secrets.yaml")
    ) %>%
    purrr::chuck(Sys.info()["user"])
} else {
  data_storage_path <-
    here::here("Data")
}

#----------------------------------------------------------#
# 5. Define variables -----
#----------------------------------------------------------#

min_age <- 0
max_age <- 12e3
timestep <- 500

# - age table for dummy data
data_dummy_time <- tibble::tibble(
  age = seq(
    from = min_age, # [config]
    to = max_age, # [config]
    by = timestep # [config]
  )
)

vec_regions <-
  c(
    "North America",
    "Latin America",
    "Europe",
    "Asia",
    "Oceania"
  ) %>%
  rlang::set_names()


# regional limits
data_regional_limits <-
  tibble::tibble(
    region = vec_regions,
    xmin = c(-170, -103, -10, 30, 110),
    xmax = c(-50, -23, 40, 175.5, 154),
    ymin = c(15, -56, 35, 0, -45),
    ymax = c(80, 34, 70, 80, -3)
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
  tibble::tibble(
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
    )
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
  c(
    90,
    180,
    270
  ) %>%
  rlang::set_names(
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
