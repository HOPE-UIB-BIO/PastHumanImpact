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
    "vfe032", "G:/My Drive/"
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

#----------------------------------------------------------#
# 6. Graphical options -----
#----------------------------------------------------------#

## examples
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

# define pallets

# define common color
