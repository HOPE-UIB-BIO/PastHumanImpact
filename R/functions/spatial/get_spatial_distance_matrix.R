#' @title Calculate great-circle distances among sites
#' @description
#' Calculate a symmetric matrix of WGS84 great-circle distances in kilometres.
#' @param data_source Data frame containing unique identifiers and coordinates.
#' @param id_col Name of the identifier column.
#' @param long_col Name of the longitude column.
#' @param lat_col Name of the latitude column.
#' @return A numeric distance matrix in kilometres with identifier dimnames.
#' @examples
#' \dontrun{
#' get_spatial_distance_matrix(data_source = metadata)
#' }
get_spatial_distance_matrix <- function(
  data_source,
  id_col = "dataset_id",
  long_col = "long",
  lat_col = "lat"
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    nrow(data_source) > 0,
    assertthat::is.string(id_col),
    assertthat::is.string(long_col),
    assertthat::is.string(lat_col)
  )

  data_coordinates <-
    validate_spatial_coordinates(
      data_source = data_source,
      id_col = id_col,
      long_col = long_col,
      lat_col = lat_col
    )

  mat_coordinates <-
    data_coordinates |>
    dplyr::select(
      dplyr::all_of(c(long_col, lat_col))
    ) |>
    as.matrix()

  mat_distance <-
    geosphere::distm(
      x = mat_coordinates,
      fun = geosphere::distGeo
    ) / 1000

  vec_ids <- as.character(data_coordinates[[id_col]])
  dimnames(mat_distance) <- list(vec_ids, vec_ids)

  return(mat_distance)
}
