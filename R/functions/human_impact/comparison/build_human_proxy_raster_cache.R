#' @title Cache selected human-proxy raster layers
#' @description
#' Materialize a small set of layers from a large external raster as a tiled,
#' compressed GeoTIFF for restartable downstream spatial extraction.
#' @param file_path Character scalar source raster path.
#' @param source_id Character scalar source identifier.
#' @param expected_layers Integer expected number of source layers.
#' @param layer_indices Integer indices to cache, in output order.
#' @param cache_path Character scalar output GeoTIFF path.
#' @param variable Optional NetCDF subdataset name.
#' @return Normalized path to the cached GeoTIFF.
build_human_proxy_raster_cache <- function(
  file_path,
  source_id,
  expected_layers,
  layer_indices,
  cache_path,
  variable = NULL
) {
  raster_source <-
    load_human_proxy_raster(
      file_path = file_path,
      source_id = source_id,
      expected_layers = expected_layers,
      variable = variable
    )

  assertthat::assert_that(
    is.numeric(layer_indices),
    length(layer_indices) > 0L,
    !anyDuplicated(layer_indices),
    all(layer_indices %in% seq_len(terra::nlyr(raster_source))),
    assertthat::is.string(cache_path),
    identical(tolower(tools::file_ext(cache_path)), "tif"),
    msg = "Human-proxy cache inputs do not satisfy the contract."
  )

  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)

  temporary_path <-
    tempfile(
      pattern = paste0(source_id, "_"),
      tmpdir = dirname(cache_path),
      fileext = ".tif"
    )

  on.exit(
    unlink(
      c(
        temporary_path,
        paste0(temporary_path, ".aux.xml")
      )
    ),
    add = TRUE
  )

  terra::writeRaster(
    raster_source[[as.integer(layer_indices)]],
    filename = temporary_path,
    overwrite = TRUE,
    datatype = "FLT4S",
    gdal = c("TILED=YES", "COMPRESS=DEFLATE", "BIGTIFF=YES")
  )

  copied <- file.copy(temporary_path, cache_path, overwrite = TRUE)

  if (!isTRUE(copied)) {
    cli::cli_abort(paste("Could not publish raster cache:", cache_path))
  }

  normalizePath(cache_path, winslash = "/", mustWork = TRUE)
}
