#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                 Execute mvpart runtime
#
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#

# Execute one serialized mvpart operation under R 3.5 without loading the
# modern project configuration. This script is launched by the modern targets
# graph and is not a standalone targets pipeline.

#----------------------------------------------------------#
# 0. Read and validate paths -----
#----------------------------------------------------------#

vec_arguments <-
  commandArgs(trailingOnly = TRUE)

if (
  length(vec_arguments) != 3L
) {
  stop("Expected request, result, and isolated-library paths.")
}

path_request <-
  vec_arguments[[1L]]

path_result <-
  vec_arguments[[2L]]

path_library <-
  vec_arguments[[3L]]

if (
  isFALSE(file.exists(path_request)) ||
    isFALSE(dir.exists(path_library))
) {
  stop("The mvpart runtime request or isolated library does not exist.")
}

.libPaths(
  c(
    path_library,
    .Library.site,
    .Library
  )
)

vec_packages <-
  c(
    "assertthat",
    "mvpart",
    "purrr"
  )

vec_available_packages <-
  c(
    assertthat = requireNamespace("assertthat", quietly = TRUE),
    mvpart = requireNamespace("mvpart", quietly = TRUE),
    purrr = requireNamespace("purrr", quietly = TRUE)
  )

vec_missing_packages <-
  vec_packages[!vec_available_packages]

if (
  length(vec_missing_packages) > 0L
) {
  stop(
    paste(
      "Missing isolated runtime packages:",
      paste(vec_missing_packages, collapse = ", ")
    )
  )
}

#----------------------------------------------------------#
# 1. Load the operation contract -----
#----------------------------------------------------------#

res_request <-
  readRDS(path_request)

purrr::walk(
  .x = res_request[["function_files"]],
  .f = ~ source(
    file = .x,
    local = .GlobalEnv,
    encoding = "UTF-8"
  )
)

operation <-
  res_request[["operation"]]

data_input <-
  res_request[["data"]]

#----------------------------------------------------------#
# 2. Execute the requested operation -----
#----------------------------------------------------------#

if (
  identical(operation, "mrt")
) {
  data_result <-
    compute_mrt(data_pollen = data_input)
} else if (
  identical(operation, "change_points")
) {
  data_result <-
    compute_pap_change_points(data_source = data_input)
} else {
  stop("Unsupported isolated mvpart operation.")
}

#----------------------------------------------------------#
# 3. Save stable data and provenance -----
#----------------------------------------------------------#

res_output <-
  list(
    data = data_result,
    provenance = list(
      operation = operation,
      r_version = as.character(getRversion()),
      mvpart_version = as.character(
        utils::packageVersion("mvpart")
      ),
      purrr_version = as.character(
        utils::packageVersion("purrr")
      ),
      assertthat_version = as.character(
        utils::packageVersion("assertthat")
      )
    )
  )

saveRDS(
  object = res_output,
  file = path_result,
  version = 2
)
