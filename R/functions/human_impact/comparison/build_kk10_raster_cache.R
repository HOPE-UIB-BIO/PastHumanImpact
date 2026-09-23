#' @title Cache selected KK10 NetCDF layers
#' @description
#' Read selected KK10 time slices directly with `ncdf4` and publish each as a
#' single-band GeoTIFF, avoiding a full GDAL scan of the 7,901-layer stack.
#' @param file_path Character scalar KK10 NetCDF path.
#' @param layer_indices Integer time indices to cache, in output order.
#' @param cache_path Character scalar base GeoTIFF path.
#' @param variable Character scalar NetCDF variable name.
#' @return Character vector of normalized cached GeoTIFF paths.
build_kk10_raster_cache <- function(
  file_path,
  layer_indices,
  cache_path,
  variable = "land_use"
) {
  assertthat::assert_that(
    assertthat::is.string(file_path),
    file.exists(file_path),
    is.numeric(layer_indices),
    length(layer_indices) > 0L,
    !anyDuplicated(layer_indices),
    all(layer_indices %in% seq_len(7901L)),
    assertthat::is.string(cache_path),
    identical(tolower(tools::file_ext(cache_path)), "tif"),
    assertthat::is.string(variable),
    msg = "KK10 raster-cache inputs do not satisfy the contract."
  )

  source_netcdf <- ncdf4::nc_open(file_path)
  on.exit(ncdf4::nc_close(source_netcdf), add = TRUE)

  source_variable <- source_netcdf[["var"]][[variable]]

  if (is.null(source_variable)) {
    cli::cli_abort(paste("KK10 variable is absent:", variable))
  }

  dimension_names <-
    vapply(
      source_variable[["dim"]],
      function(dimension) dimension[["name"]],
      character(1)
    )
  dimension_lengths <-
    vapply(
      source_variable[["dim"]],
      function(dimension) dimension[["len"]],
      numeric(1)
    )

  assertthat::assert_that(
    identical(dimension_names, c("lon", "lat", "time")),
    identical(as.numeric(dimension_lengths), c(4320, 2160, 7901)),
    msg = "KK10 NetCDF dimensions do not match lon-lat-time expectations."
  )

  longitude <- source_variable[["dim"]][[1]][["vals"]]
  latitude <- source_variable[["dim"]][[2]][["vals"]]
  longitude_resolution <- stats::median(diff(longitude))
  latitude_resolution <- stats::median(diff(latitude))

  raster_template <-
    terra::rast(
      nrows = length(latitude),
      ncols = length(longitude),
      xmin = min(longitude) - longitude_resolution / 2,
      xmax = max(longitude) + longitude_resolution / 2,
      ymin = min(latitude) - latitude_resolution / 2,
      ymax = max(latitude) + latitude_resolution / 2,
      crs = "EPSG:4326"
    )

  cache_stem <- tools::file_path_sans_ext(cache_path)
  cache_paths <-
    sprintf(
      "%s_%04d.tif",
      cache_stem,
      seq_along(layer_indices)
    )

  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)

  purrr::walk2(
    layer_indices,
    cache_paths,
    function(layer_index, output_path) {
      cli::cli_inform(
        c("i" = paste("Caching KK10 source layer", layer_index))
      )

      source_values <-
        ncdf4::ncvar_get(
          source_netcdf,
          varid = variable,
          start = c(1L, 1L, as.integer(layer_index)),
          count = c(-1L, -1L, 1L),
          collapse_degen = TRUE
        )

      output_raster <- raster_template
      terra::values(output_raster) <-
        as.vector(
          source_values[, rev(seq_len(ncol(source_values))), drop = FALSE]
        )

      temporary_path <-
        tempfile(
          pattern = "kk10_layer_",
          tmpdir = dirname(cache_path),
          fileext = ".tif"
        )
      on.exit(unlink(temporary_path), add = TRUE, after = FALSE)

      terra::writeRaster(
        output_raster,
        filename = temporary_path,
        overwrite = TRUE,
        datatype = "FLT4S",
        gdal = c("TILED=YES", "COMPRESS=DEFLATE")
      )

      if (!isTRUE(file.copy(temporary_path, output_path, overwrite = TRUE))) {
        cli::cli_abort(paste("Could not publish KK10 cache:", output_path))
      }

      unlink(temporary_path)
    }
  )

  normalizePath(cache_paths, winslash = "/", mustWork = TRUE)
}
