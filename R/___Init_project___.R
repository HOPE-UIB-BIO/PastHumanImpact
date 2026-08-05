#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                     Project setup
#
#
#                   V. Felde, O. Mottl
#                         2024
#
#----------------------------------------------------------#

# Prepare the reproducible R environment for a fresh project checkout.
# Run this script from the project root. The root `renv.lock` is the
# authoritative package specification.
#
# This initializer deliberately does not install individual project packages
# or create a snapshot. `renv::restore()` installs the exact locked versions;
# snapshotting is a separate, intentional maintenance operation.


#----------------------------------------------------------#
# 0. Bootstrap renv -----
#----------------------------------------------------------#

if (
  isFALSE(
    requireNamespace(
      package = "renv",
      quietly = TRUE
    )
  )
) {
  message("Package {renv} is not installed; installing it from CRAN.")
  utils::install.packages(
    pkgs = "renv"
  )
}

if (
  isFALSE(
    requireNamespace(
      package = "renv",
      quietly = TRUE
    )
  )
) {
  stop(
    "Package {renv} could not be installed; project setup cannot continue.",
    call. = FALSE
  )
}


#----------------------------------------------------------#
# 1. Locate the project and lockfile -----
#----------------------------------------------------------#

path_project <-
  normalizePath(
    path = ".",
    winslash = "/",
    mustWork = TRUE
  )

path_lockfile <-
  file.path(
    path_project,
    "renv.lock"
  )

if (
  isFALSE(
    file.exists(path_lockfile)
  )
) {
  stop(
    "The root renv.lock was not found at ",
    path_lockfile,
    ". Run this script from the project root.",
    call. = FALSE
  )
}


#----------------------------------------------------------#
# 2. Activate the project library -----
#----------------------------------------------------------#

message(
  "Activating the renv project at ",
  path_project,
  "."
)

renv::activate(
  project = path_project
)

# In an interactive session, renv may request an R restart after first
# activation. If so, restart R and run this script once more.


#----------------------------------------------------------#
# 3. Restore the locked package versions -----
#----------------------------------------------------------#

message(
  "Restoring packages from ",
  path_lockfile,
  "."
)

renv::restore(
  project = path_project,
  lockfile = path_lockfile,
  prompt = FALSE
)


#----------------------------------------------------------#
# 4. Verify the restored environment -----
#----------------------------------------------------------#

res_project_status <-
  renv::status(
    project = path_project,
    lockfile = path_lockfile
  )

if (
  isFALSE(
    isTRUE(
      res_project_status[["synchronized"]]
    )
  )
) {
  stop(
    "The restored project library is not synchronized with renv.lock.",
    call. = FALSE
  )
}

message("Project setup completed; the renv library matches renv.lock.")


#----------------------------------------------------------#
# Lockfile maintenance -----
#----------------------------------------------------------#

# Package changes are declared in `package_list` in `R/00_Config_file.R`.
# Sourcing that configuration regenerates `R/package_dependencies.R`.
# After an intentional package change, review the environment and update the
# root lockfile explicitly with:
#
# renv::snapshot(
#   project = path_project,
#   lockfile = path_lockfile,
#   prompt = FALSE
# )
