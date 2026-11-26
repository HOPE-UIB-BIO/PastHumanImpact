#' @title Save HYDE land-use fraction layers as a raster cache
#' @description
#' Compute the HYDE land-use fraction, (cropland + grazing land) / cell
#' area, for selected time layers and save it as one tiled, compressed
#' GeoTIFF for spatial extraction.
#' @param path_cropland
#' Character scalar path to a cropland raster (km2 per cell, one layer per
#' time step).
#' @param path_grazing
#' Character scalar path to a grazing-land raster with the same geometry and
#' time steps as `path_cropland`.
#' @param path_cell_area
#' Character scalar path to a one-layer raster of total cell area (km2) on
#' the same grid.
#' @param layer_indices
#' Integer vector of distinct time-layer indices to keep, in output order.
#' @param cache_path
#' Character scalar output path ending in `.tif`.
#' @return
#' Normalised path to the saved GeoTIFF, with one layer per element of
#' `layer_indices`.
#' @details
#' Cells where cropland, grazing land, or cell area is missing, or where the
#' cell area is not positive, are missing in the output. Rasters with a
#' different extent, resolution, or number of time layers are an error. The
#' output keeps the coordinate reference system of the cropland raster.
#' @examples
#' \dontrun{
#' save_hyde_land_use_fraction_cache(
#'   "cropland.nc", "grazing_land.nc", "garea_cr.asc", 1:3, "lu.tif"
#' )
#' }
save_hyde_land_use_fraction_cache <- function(
  path_cropland,
  path_grazing,
  path_cell_area,
  layer_indices,
  cache_path
) {
  assertthat::assert_that(
    assertthat::is.string(path_cropland),
    assertthat::is.string(path_grazing),
    assertthat::is.string(path_cell_area),
    file.exists(path_cropland),
    file.exists(path_grazing),
    file.exists(path_cell_area),
    is.numeric(layer_indices),
    length(layer_indices) > 0L,
    all(is.finite(layer_indices)),
    all(layer_indices == round(layer_indices)),
    !anyDuplicated(layer_indices),
    assertthat::is.string(cache_path),
    identical(tolower(tools::file_ext(cache_path)), "tif"),
    msg = "HYDE land-use cache inputs do not satisfy the contract."
  )

  raster_cropland <-
    terra::rast(path_cropland)

  raster_grazing <-
    terra::rast(path_grazing)

  raster_area <-
    terra::rast(path_cell_area)

  flag_same_grid <-
    terra::nlyr(raster_cropland) == terra::nlyr(raster_grazing) &&
    terra::nlyr(raster_area) == 1L &&
    terra::compareGeom(
      raster_cropland,
      raster_grazing,
      crs = FALSE,
      stopOnError = FALSE
    ) &&
    terra::compareGeom(
      raster_cropland,
      raster_area,
      crs = FALSE,
      stopOnError = FALSE
    )

  if (
    !isTRUE(flag_same_grid)
  ) {
    cli::cli_abort(
      c(
        "HYDE cropland, grazing, and cell-area rasters do not share one grid.",
        "i" = stringr::str_c(
          "They need the same extent and resolution, equal time layers for ",
          "cropland and grazing, and one cell-area layer."
        )
      )
    )
  }

  assertthat::assert_that(
    all(layer_indices %in% seq_len(terra::nlyr(raster_cropland))),
    msg = "HYDE land-use cache inputs do not satisfy the contract."
  )

  vec_layer_indices <-
    as.integer(layer_indices)

  # The ASCII cell-area grid may carry no coordinate system; the grids are
  #   checked above, so it takes the cropland system.
  raster_area_referenced <- raster_area

  terra::crs(raster_area_referenced) <- terra::crs(raster_cropland)

  raster_area_positive <-
    terra::ifel(
      raster_area_referenced > 0,
      raster_area_referenced,
      NA
    )

  raster_fraction <-
    (raster_cropland[[vec_layer_indices]] +
      raster_grazing[[vec_layer_indices]]) /
    raster_area_positive

  names(raster_fraction) <-
    stringr::str_c("land_use_fraction_", vec_layer_indices)

  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)

  path_temporary <-
    tempfile(
      pattern = "hyde_land_use_",
      tmpdir = dirname(cache_path),
      fileext = ".tif"
    )

  on.exit(
    unlink(
      c(
        path_temporary,
        stringr::str_c(path_temporary, ".aux.xml")
      )
    ),
    add = TRUE
  )

  terra::writeRaster(
    raster_fraction,
    filename = path_temporary,
    overwrite = TRUE,
    datatype = "FLT4S",
    gdal = c("TILED=YES", "COMPRESS=DEFLATE", "BIGTIFF=YES")
  )

  if (
    !isTRUE(file.copy(path_temporary, cache_path, overwrite = TRUE))
  ) {
    cli::cli_abort("Could not save the HYDE land-use cache {.path {cache_path}}.")
  }

  res_path <-
    normalizePath(cache_path, winslash = "/", mustWork = TRUE)

  return(res_path)
}
