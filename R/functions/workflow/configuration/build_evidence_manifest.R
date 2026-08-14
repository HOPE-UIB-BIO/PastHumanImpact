#' @title Build an analysis evidence manifest
#' @description
#' Validate stable analysis artifacts and attach file hashes and creation
#' metadata for manuscript and response-document traceability.
#' @param data_artifacts Data frame describing expected analysis artifacts.
#' @return Tibble containing artifact descriptions, paths, hashes, and status.
#' @examples
#' \dontrun{
#' manifest <- build_evidence_manifest(data_artifacts = artifacts)
#' }
build_evidence_manifest <- function(data_artifacts) {
  required_columns <-
    c(
      "artifact_id",
      "description",
      "analysis_profile",
      "source_pipeline",
      "public_target",
      "path"
    )

  assertthat::assert_that(
    is.data.frame(data_artifacts),
    all(required_columns %in% names(data_artifacts)),
    !anyDuplicated(data_artifacts[["artifact_id"]]),
    msg = "Evidence artifacts must have unique IDs and required columns."
  )

  res_manifest <-
    data_artifacts |>
    dplyr::mutate(
      file_exists = file.exists(.data[["path"]]),
      file_hash = purrr::map_chr(
        .data[["path"]],
        ~ if (
          file.exists(.x)
        ) {
          unname(tools::md5sum(.x))
        } else {
          NA_character_
        }
      ),
      file_modified = purrr::map_chr(
        .data[["path"]],
        ~ if (
          file.exists(.x)
        ) {
          as.character(file.info(.x)[["mtime"]])
        } else {
          NA_character_
        }
      ),
      validation_status = dplyr::if_else(
        .data[["file_exists"]],
        "available",
        "missing"
      )
    )

  return(res_manifest)
}
