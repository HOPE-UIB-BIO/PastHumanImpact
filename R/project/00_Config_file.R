#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                        Config File
#
#
#                   O. Mottl, V.A. Felde
#                         2023
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
    "data.tree",
    "furrr",
    "future",
    "geosphere",
    "ggeffects",
    "ggforce",
    "ggimage",
    "ggpubr",
    "here",
    "httpgd",
    "insight",
    "janitor",
    "jsonlite",
    "kableExtra",
    "knitr",
    "languageserver",
    "mgcv",
    "parallelly",
    "rcarbon",
    "rdacca.hp",
    "REcopol",
    "renv",
    "remotes",
    "rlang",
    "RRatepol",
    "RUtilpol",
    "tarchetypes",
    "targets",
    "terra",
    "tidyverse",
    "usethis",
    "utils",
    "vegan",
    "venneuler"
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
# 4. Authorise the user -----
#----------------------------------------------------------#

# Define directory for external storage for users
auth_tibble <-
  tibble::tribble(
    ~name, ~path,
    "ondrej", "C:/Users/ondre/My Drive/",
    "vfe032", "G:/My Drive/",
    "ondre", "H:/My Drive/",
    "vivia", "H:/Min disk/"
  )

data_storage_path <-
  auth_tibble %>%
  dplyr::filter(name == Sys.info()["user"]) %>%
  purrr::pluck("path")


# external_storage_targets <-
#   paste0(
#     data_storage_path,
#     "_targets_data"
#   )

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

data_climate_zones <-
  tibble::tibble(
    climatezone = factor(
      c(
        "Arid",
        "Cold_Dry_Summer",
        "Cold_Dry_Winter",
        "Cold_Without_dry_season_Cold_Summer",
        "Cold_Without_dry_season_Hot_Summer",
        "Cold_Without_dry_season_Very_Cold_Summer",
        "Cold_Without_dry_season_Warm_Summer",
        "Polar",
        "Temperate_Dry_Summer",
        "Temperate_Dry_Winter",
        "Temperate_Without_dry_season",
        "Tropical"
      ),
      levels = c(
        "Polar",
        "Cold_Without_dry_season_Very_Cold_Summer",
        "Cold_Without_dry_season_Cold_Summer",
        "Cold_Without_dry_season_Warm_Summer",
        "Cold_Without_dry_season_Hot_Summer",
        "Cold_Dry_Winter",
        "Cold_Dry_Summer",
        "Temperate_Without_dry_season",
        "Temperate_Dry_Winter",
        "Temperate_Dry_Summer",
        "Tropical",
        "Arid"
      )
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

# Define colour palette

# Ecozones
palette_ecozones <-
  c(
    Polar = "#946dff",
    Cold_Without_dry_season_Very_Cold_Summer = "#C9D9FF",
    Cold_Without_dry_season_Cold_Summer = "#4CABFF",
    Cold_Without_dry_season_Warm_Summer = "#3E517A",
    Cold_Without_dry_season_Hot_Summer = "#012144",
    Cold_Dry_Winter = "#006138",
    Cold_Dry_Summer = "#863B28",
    Temperate_Without_dry_season = "#867718",
    Temperate_Dry_Winter = "#96E6B3",
    Temperate_Dry_Summer = "#BEEE62",
    Arid = "#EFE31E",
    Tropical = "#D81E5B"
  )

# Predictors
palette_predictors <- c(
  human = "grey50", # v#ffa600"
  climate = "grey50" # v#d74e92"
)


# predictor parts
palette_predictors_parts <-
  c(
    "#9C8A6C",
    "#BEB689",
    "#DEDEB6"
  ) %>%
  rlang::set_names(
    nm = c(
      "unique_percent",
      "average_share_percent",
      "individual_percent"
    )
  )

# Parameters
predictors_spatial_order <- c("human", "time", "climate")
predictors_label <- c("Human", "Time", "Climate")

# define common color
