#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                        Config File
#
#
#                   O. Mottl, V. Felde
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
    "arrow",
    "assertthat",
    "colorspace",
    "data.tree",
    "geosphere",
    "ggpubr",
    "ggforce",
    "ggimage",
    "here",
    "httpgd",
    "janitor",
    "jsonlite",
    "kableExtra",
    "knitr",
    "languageserver",
    "lifecycle",
    "mgcv",
    "rcarbon",
    "rdacca.hp",
    "REcopol",
    "renv",
    "remotes",
    "rlang",
    "roxygen2",
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
    "ondre", "H:/My Drive/"
  )

data_storage_path <-
  auth_tibble %>%
  dplyr::filter(name == Sys.info()["user"]) %>%
  purrr::pluck("path")


external_storage_targets <-
  paste0(
    data_storage_path,
    "_targets_h1"
  )

#----------------------------------------------------------#
# 5. Define variables -----
#----------------------------------------------------------#

min_age <- 0
max_age <- 12e3
timestep <- 500

# regional limits
data_regional_limits <-
  tibble::tibble(
    region = c("North America", "Europe", "Asia", "Latin America", "Oceania"),
    xmin = c(-170, -10, 30, -103, 110),
    xmax = c(-50, 50, 180, -23, 154),
    ymin = c(10, 30, 0, -56, -50),
    ymax = c(89, 80, 80, 34, -3)
  )

data_climate_zones <-
  tibble::tibble(
    sel_classification = factor(
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
        "Cold_Dry_Winter",
        "Cold_Dry_Summer",
        "Cold_Without_dry_season_Very_Cold_Summer",
        "Cold_Without_dry_season_Cold_Summer",
        "Cold_Without_dry_season_Warm_Summer",
        "Cold_Without_dry_season_Hot_Summer",
        "Temperate_Dry_Winter",
        "Temperate_Dry_Summer",
        "Temperate_Without_dry_season",
        "Tropical",
        "Arid"
      )
    )
  )


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
image_width <- 16
image_height <- 12
image_units <- "cm"

# Define colour palette

# Ecozones
palette_ecozones <-
  c(
    Polar = "#009292",
    Cold_Without_dry_season_Very_Cold_Summer = "#004949",
    Cold_Without_dry_season_Cold_Summer = "#006ddb",
    Cold_Dry_Winter = "#6db6ff",
    Cold_Dry_Summer = "#b6dbff",
    Cold_Without_dry_season_Warm_Summer = "#117733",
    Cold_Without_dry_season_Hot_Summer = "#999933",
    Temperate_Without_dry_season = "#DDCC77",
    Temperate_Dry_Winter = "#b66dff",
    Temperate_Dry_Summer = "#ffff6d",
    Arid = "#924900",
    Tropical = "#920000"
  )

# Predictors
palette_predictors <- c(
  human = "#663333",
  climate = "#4a2577"
)

# predictor parts
palette_predictors_parts <-
  c(
    "maroon4",
    "grey70",
    "grey30"
  ) %>%
  rlang::set_names(
    nm = c(
      "Unique_percent",
      "Average.share_percent",
      "Individual_percent"
    )
  )

# Parameters
predictors_spatial_order <- c("human", "time", "climate")
predictors_label <- c("Human", "Time", "Climate")

# define common color
