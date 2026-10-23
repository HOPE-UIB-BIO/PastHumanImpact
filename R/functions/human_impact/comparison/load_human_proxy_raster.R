#' @title Load an external human-proxy raster
#' @description
#' Load a raster source and validate its documented layer count and geographic
#' coordinate reference system.
#' @param file_path Character scalar path to a raster file.
#' @param source_id Character scalar source identifier.
#' @param expected_layers Integer expected number of raster layers.
#' @param variable Optional NetCDF subdataset name.
#' @return A `SpatRaster`.
#' @examples
#' \dontrun{
#' raster <- load_human_proxy_raster("KK10.nc", "kk10", 7901L, "land_use")
#' }
load_human_proxy_raster <- function(
  file_path,
  source_id,
  expected_layers,
  variable = NULL
) {
  assertthat::assert_that(
    is.character(file_path),
    length(file_path) > 0L,
    all(file.exists(file_path)),
    assertthat::is.string(source_id),
    is.numeric(expected_layers),
    length(expected_layers) == 1L,
    expected_layers > 0L,
    is.null(variable) ||
      (assertthat::is.string(variable) && length(file_path) == 1L),
    msg = "Human-proxy raster inputs do not satisfy the contract."
  )

  res_raster <-
    if (
      is.null(variable)
    ) {
      terra::rast(file_path)
    } else {
      tryCatch(
        terra::rast(file_path, subds = variable),
        error = function(err) {
          cli::cli_abort(
            paste(
              "Could not load subdataset",
              variable,
              "from",
              basename(file_path),
              ":",
              conditionMessage(err)
            )
          )
        }
      )
    }

  if (
    terra::nlyr(res_raster) != as.integer(expected_layers)
  ) {
    cli::cli_abort(
      paste(
        source_id,
        "contains",
        terra::nlyr(res_raster),
        "layers; expected",
        as.integer(expected_layers),
        "."
      )
    )
  }

  if (
    !terra::is.lonlat(res_raster)
  ) {
    cli::cli_abort(
      paste(source_id, "must use a longitude-latitude coordinate system.")
    )
  }

  return(res_raster)
}
