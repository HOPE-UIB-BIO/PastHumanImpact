#' @title Run external human-proxy source acquisition
#' @description
#' Download the documented KK10 and HYDE archives, prepare the HYDE population
#' raster, and validate final raster layer and coordinate-system contracts.
#' @param data_sources Human-proxy source manifest.
#' @param data_storage_path Character scalar project data root.
#' @param overwrite Logical. Restart downloads and replace completed files.
#' @param validate_rasters Logical. Open final rasters and validate their layer
#'   counts and geographic coordinate reference systems.
#' @return Validated source manifest with file sizes.
#' @examples
#' \dontrun{
#' run_human_proxy_source_acquisition(sources, "Data")
#' }
run_human_proxy_source_acquisition <- function(
  data_sources,
  data_storage_path,
  overwrite = FALSE,
  validate_rasters = TRUE
) {
  assertthat::assert_that(
    is.data.frame(data_sources),
    "download_url" %in% names(data_sources),
    is.logical(overwrite),
    length(overwrite) == 1L,
    is.logical(validate_rasters),
    length(validate_rasters) == 1L,
    msg = "Human-proxy acquisition inputs do not satisfy the contract."
  )

  data_resolved <-
    validate_human_proxy_source_manifest(
      data_sources = data_sources,
      data_storage_path = data_storage_path,
      require_files = FALSE
    )

  source_kk10 <-
    data_resolved |>
    dplyr::filter(.data[["source_id"]] == "kk10")
  source_hyde <-
    data_resolved |>
    dplyr::filter(.data[["source_id"]] == "hyde_3_2")

  save_resumable_url_file(
    url = source_kk10[["download_url"]][[1]],
    destination = source_kk10[["file_path"]][[1]],
    overwrite = overwrite
  )

  if (
    !file.exists(source_hyde[["file_path"]][[1]]) || isTRUE(overwrite)
  ) {
    path_hyde_archive <-
      file.path(
        dirname(source_hyde[["file_path"]][[1]]),
        "raw-data.zip"
      )

    save_resumable_url_file(
      url = source_hyde[["download_url"]][[1]],
      destination = path_hyde_archive,
      overwrite = overwrite
    )

    prepare_hyde_population_raster(
      archive_path = path_hyde_archive,
      destination = source_hyde[["file_path"]][[1]],
      overwrite = overwrite
    )
  }

  res_sources <-
    validate_human_proxy_source_manifest(
      data_sources = data_sources,
      data_storage_path = data_storage_path,
      require_files = TRUE
    )

  if (
    isTRUE(validate_rasters)
  ) {
    purrr::walk(
      seq_len(nrow(res_sources)),
      ~ {
        source_id <- res_sources[["source_id"]][[.x]]
        source_raster <-
          load_human_proxy_raster(
            file_path = res_sources[["file_path"]][[.x]],
            source_id = source_id,
            expected_layers = res_sources[["expected_layers"]][[.x]],
            variable = if (identical(source_id, "kk10")) {
              res_sources[["variable"]][[.x]]
            } else {
              NULL
            }
          )
        rm(source_raster)
        invisible(gc())
      }
    )
  }

  res_sources <-
    res_sources |>
    dplyr::mutate(
      file_size_bytes = file.info(.data[["file_path"]])[["size"]]
    )

  return(res_sources)
}
