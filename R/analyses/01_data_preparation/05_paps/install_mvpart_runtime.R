#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                Isolated mvpart runtime
#
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#

# Install the archived mvpart package without loading the modern project
# environment or its dependency set.
#
# Resolve this script, then run it from outside the repository so the project
# .Rprofile cannot activate the modern renv environment:
#
# $script = (Resolve-Path `
#   "R/analyses/01_data_preparation/05_paps/install_mvpart_runtime.R").Path
# Push-Location ([System.IO.Path]::GetTempPath())
# rig run --r-version 3.5.0 --script $script
# Pop-Location
#
# An optional first trailing argument selects the library directory. The
# default is ~/R/legacy-library/mvpart-r35.

#----------------------------------------------------------#
# 0. Read and validate configuration -----
#----------------------------------------------------------#

vec_arguments <-
  commandArgs(trailingOnly = TRUE)

if (
  "renv" %in% loadedNamespaces()
) {
  warning(
    paste(
      "renv was activated before this script started.",
      "Run the rig command from outside the repository."
    )
  )
}

if (
  length(vec_arguments) > 1L
) {
  stop("Supply at most one library-directory argument.")
}

if (
  getRversion() >= numeric_version("4.3.0")
) {
  stop(
    paste(
      "mvpart 1.6-2 does not compile with R 4.3 or newer.",
      "Run this script with `rig run --r-version 3.5.0`."
    )
  )
}

path_library <-
  if (
    length(vec_arguments) == 1L
  ) {
    path.expand(vec_arguments[[1L]])
  } else {
    path.expand("~/R/legacy-library/mvpart-r35")
  }

dir.create(
  path = path_library,
  recursive = TRUE,
  showWarnings = FALSE
)

#----------------------------------------------------------#
# 1. Install the pinned source revisions -----
#----------------------------------------------------------#

assertthat_revision <-
  "b28a7b86e920506642df3d16aee1a6f231af581f"

magrittr_revision <-
  "6180ab5ec5619b97f9654387111b64c8de774de4"

rlang_revision <-
  "8bf1a3e2a49c249d2a50a830b530b6e42add22ee"

purrr_revision <-
  "5bb5aa5974b3858cbf8f9402e5a4792997496aab"

mvpart_revision <-
  "acf0035fc7d002da46e4df5e4f4a77f04432f40e"

assertthat_source_url <-
  paste0(
    "https://github.com/cran/assertthat/archive/",
    assertthat_revision,
    ".tar.gz"
  )

magrittr_source_url <-
  paste0(
    "https://github.com/cran/magrittr/archive/",
    magrittr_revision,
    ".tar.gz"
  )

rlang_source_url <-
  paste0(
    "https://github.com/cran/rlang/archive/",
    rlang_revision,
    ".tar.gz"
  )

purrr_source_url <-
  paste0(
    "https://github.com/cran/purrr/archive/",
    purrr_revision,
    ".tar.gz"
  )

mvpart_source_url <-
  paste0(
    "https://github.com/cran/mvpart/archive/",
    mvpart_revision,
    ".tar.gz"
  )

path_assertthat_archive <-
  tempfile(fileext = ".tar.gz")

path_magrittr_archive <-
  tempfile(fileext = ".tar.gz")

path_rlang_archive <-
  tempfile(fileext = ".tar.gz")

path_purrr_archive <-
  tempfile(fileext = ".tar.gz")

path_mvpart_archive <-
  tempfile(fileext = ".tar.gz")

utils::download.file(
  url = assertthat_source_url,
  destfile = path_assertthat_archive,
  mode = "wb"
)

utils::download.file(
  url = magrittr_source_url,
  destfile = path_magrittr_archive,
  mode = "wb"
)

utils::download.file(
  url = rlang_source_url,
  destfile = path_rlang_archive,
  mode = "wb"
)

utils::download.file(
  url = purrr_source_url,
  destfile = path_purrr_archive,
  mode = "wb"
)

utils::download.file(
  url = mvpart_source_url,
  destfile = path_mvpart_archive,
  mode = "wb"
)

utils::install.packages(
  pkgs = c(
    path_assertthat_archive,
    path_magrittr_archive,
    path_rlang_archive,
    path_purrr_archive,
    path_mvpart_archive
  ),
  lib = path_library,
  repos = NULL,
  type = "source",
  INSTALL_opts = "--no-lock"
)

unlink(
  c(
    path_assertthat_archive,
    path_magrittr_archive,
    path_rlang_archive,
    path_purrr_archive,
    path_mvpart_archive
  )
)

#----------------------------------------------------------#
# 2. Verify the isolated library -----
#----------------------------------------------------------#

.libPaths(
  c(path_library, .libPaths())
)

vec_available_packages <-
  c(
    assertthat = requireNamespace("assertthat", quietly = TRUE),
    magrittr = requireNamespace("magrittr", quietly = TRUE),
    mvpart = requireNamespace("mvpart", quietly = TRUE),
    purrr = requireNamespace("purrr", quietly = TRUE),
    rlang = requireNamespace("rlang", quietly = TRUE)
  )

if (
  any(!vec_available_packages)
) {
  stop("One or more isolated runtime packages were unavailable.")
}

message(
  paste0(
    "Installed mvpart ",
    as.character(utils::packageVersion("mvpart")),
    " and purrr ",
    as.character(utils::packageVersion("purrr")),
    " with R ",
    as.character(getRversion()),
    " in ",
    normalizePath(path_library, winslash = "/")
  )
)
