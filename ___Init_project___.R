#----------------------------------------------------------#
#
#
#                       Hypothesis I
#
#                     Project setup
#
#
#                   V. Felde, O. Mottl
#                         2022
#
#----------------------------------------------------------#

# Script to prepare all necessary components of environment to run the Project.
#   Needs to be run only once


#----------------------------------------------------------#
# Step 0: Define package list and custom function -----
#----------------------------------------------------------#

# list of all CRAN packages
package_list <-
  c(
    "arrow",
    "assertthat",
    "devtools",
    "geosphere",
    "ggpubr",
    "here",
    "mgcv",
    "rcarbon",
    "readr",
    "renv",
    "roxygen2",
    "terra",
    "tidyverse",
    "usethis",
    "vegan"
  )

# define helper function
install_packages <-
  function(pkgs_list) {
    # install all packages in the lst from CRAN
    sapply(pkgs_list, utils::install.packages, character.only = TRUE)

    # install RUtilpol from GitHub
    devtools::install_github("HOPE-UIB-BIO/R-Utilpol-package",
      quiet = FALSE,
      upgrade = FALSE
    )

    # install RRatepol from GitHub
    devtools::install_github(
      "HOPE-UIB-BIO/R-Ratepol-package",
      quiet = FALSE,
      upgrade = FALSE
    )

    # install REcolpol from GitHub
    devtools::install_github(
      "HOPE-UIB-BIO/R-Ecopol-package",
      quiet = FALSE,
      upgrade = FALSE
    )
  }


#----------------------------------------------------------#
# Step 1: Install 'renv' package -----
#----------------------------------------------------------#

utils::install.packages("renv")


#----------------------------------------------------------#
# Step 2: Deactivate 'renv' package -----
#----------------------------------------------------------#

# deactivate to make sure that packages are updated on the machine
renv::deactivate()


#----------------------------------------------------------#
# Step 3: Install packages to the machine
#----------------------------------------------------------#

install_packages(package_list)


#----------------------------------------------------------#
# Step 4: Activate 'renv' project
#----------------------------------------------------------#

renv::activate()


#----------------------------------------------------------#
# Step 5: Install packages to the project
#----------------------------------------------------------#

install_packages(package_list)


#----------------------------------------------------------#
# Step 6: Synchronize package versions with the project
#----------------------------------------------------------#

library(here)
renv::restore()


#----------------------------------------------------------#
# Step 7: Run the project
#----------------------------------------------------------#
