#' @title Assemble revision-local publication assets
#' @description
#' Copy the registered publication figures into a manuscript revision project
#' and write a source-to-destination hash manifest.
#' @param registry_path Character scalar path to the claim/evidence registry.
#' @param project_dir Character scalar path to the revision Quarto project.
#' @param repository_dir Character scalar path to the repository root.
#' @param manifest_path Character scalar path for the generated artifact
#'   manifest.
#' @return Tibble describing copied publication assets and their hashes.
#' @examples
#' \dontrun{
#' build_revision_assets(
#'   registry_path = "Manuscript/COMMSENV-25-2408/R1/evidence/claim-evidence-registry.csv",
#'   project_dir = "Manuscript/COMMSENV-25-2408/R1",
#'   repository_dir = ".",
#'   manifest_path = "Manuscript/COMMSENV-25-2408/R1/evidence/revision-artifact-manifest.csv"
#' )
#' }
build_revision_assets <- function(
  registry_path,
  project_dir,
  repository_dir,
  manifest_path
) {
  required_columns <-
    c(
      "artifact_id",
      "path",
      "figure_destination",
      "validation_status"
    )

  registry <-
    readr::read_csv(
      registry_path,
      show_col_types = FALSE
    )

  assertthat::assert_that(
    all(required_columns %in% names(registry)),
    !anyDuplicated(registry[["artifact_id"]]),
    all(registry[["validation_status"]] == "registered"),
    msg = "Revision asset registry is invalid."
  )

  assets <-
    registry |>
    dplyr::filter(
      !is.na(.data[["figure_destination"]]),
      nzchar(.data[["figure_destination"]])
    )

  assertthat::assert_that(
    nrow(assets) > 0L,
    !anyDuplicated(assets[["path"]]),
    !anyDuplicated(assets[["figure_destination"]]),
    msg = "Revision assets must have unique sources and destinations."
  )

  repository_dir <-
    normalizePath(repository_dir, winslash = "/", mustWork = TRUE)

  project_dir <-
    normalizePath(project_dir, winslash = "/", mustWork = TRUE)

  figures_dir <-
    normalizePath(
      file.path(project_dir, "figures"),
      winslash = "/",
      mustWork = TRUE
    )

  assets <-
    assets |>
    dplyr::mutate(
      source_absolute = file.path(repository_dir, .data[["path"]]),
      destination_absolute = file.path(
        project_dir,
        .data[["figure_destination"]]
      ),
      destination_normalized = normalizePath(
        .data[["destination_absolute"]],
        winslash = "/",
        mustWork = FALSE
      )
    )

  figures_prefix <-
    paste0(tolower(figures_dir), "/")

  assertthat::assert_that(
    all(file.exists(assets[["source_absolute"]])),
    all(startsWith(
      tolower(assets[["destination_normalized"]]),
      figures_prefix
    )),
    msg = "Revision assets are missing or have unsafe destinations."
  )

  existing_assets <-
    list.files(
      figures_dir,
      recursive = TRUE,
      full.names = TRUE
    ) |>
    normalizePath(
      winslash = "/",
      mustWork = FALSE
    )

  unexpected_assets <-
    setdiff(
      tolower(existing_assets),
      tolower(assets[["destination_normalized"]])
    )

  assertthat::assert_that(
    length(unexpected_assets) == 0L,
    msg = paste(
      "Revision figures directory contains unregistered files:",
      paste(unexpected_assets, collapse = ", ")
    )
  )

  purrr::walk(
    unique(dirname(assets[["destination_absolute"]])),
    ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
  )

  copy_status <-
    purrr::map2_lgl(
      assets[["source_absolute"]],
      assets[["destination_absolute"]],
      ~ file.copy(.x, .y, overwrite = TRUE, copy.mode = TRUE)
    )

  assertthat::assert_that(
    all(copy_status),
    all(file.exists(assets[["destination_absolute"]])),
    msg = "One or more revision assets could not be copied."
  )

  result <-
    assets |>
    dplyr::transmute(
      artifact_id = .data[["artifact_id"]],
      source_path = .data[["path"]],
      destination_path = .data[["figure_destination"]],
      source_hash = unname(tools::md5sum(.data[["source_absolute"]])),
      destination_hash = unname(
        tools::md5sum(.data[["destination_absolute"]])
      ),
      validation_status = dplyr::if_else(
        .data[["source_hash"]] == .data[["destination_hash"]],
        "validated",
        "hash_mismatch"
      )
    )

  assertthat::assert_that(
    all(result[["validation_status"]] == "validated"),
    msg = "Revision asset hashes do not match their sources."
  )

  dir.create(
    dirname(manifest_path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  readr::write_csv(result, manifest_path)

  return(result)
}
