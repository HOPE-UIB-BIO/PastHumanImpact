#' @title Validate and identify spatial locations
#' @description
#' Validate site identifiers and geographic coordinates, then identify records
#' that share an exact coordinate within an optional spatial group.
#' @param data_source Data frame containing identifiers and coordinates.
#' @param id_col Name of the unique record identifier column.
#' @param long_col Name of the longitude column in decimal degrees.
#' @param lat_col Name of the latitude column in decimal degrees.
#' @param group_col Optional name of a grouping column.
#' @return
#' The input data with `spatial_location_id` and `n_colocated` columns.
#' @examples
#' \dontrun{
#' validate_spatial_coordinates(
#'   data_source = metadata,
#'   group_col = "region"
#' )
#' }
validate_spatial_coordinates <- function(
  data_source,
  id_col = "dataset_id",
  long_col = "long",
  lat_col = "lat",
  group_col = NULL
) {
  required_columns <- c(id_col, long_col, lat_col, group_col)

  assertthat::assert_that(
    is.data.frame(data_source),
    is.character(id_col),
    length(id_col) == 1L,
    is.character(long_col),
    length(long_col) == 1L,
    is.character(lat_col),
    length(lat_col) == 1L,
    is.null(group_col) ||
      (is.character(group_col) && length(group_col) == 1L),
    all(required_columns %in% names(data_source)),
    msg = "Spatial coordinate inputs do not satisfy the required contract."
  )
  assertthat::assert_that(
    nrow(data_source) > 0L,
    !anyNA(data_source[[id_col]]),
    !anyDuplicated(data_source[[id_col]]),
    is.numeric(data_source[[long_col]]),
    is.numeric(data_source[[lat_col]]),
    all(is.finite(data_source[[long_col]])),
    all(is.finite(data_source[[lat_col]])),
    all(dplyr::between(data_source[[long_col]], -180, 180)),
    all(dplyr::between(data_source[[lat_col]], -90, 90)),
    msg = "Identifiers must be unique and coordinates finite and valid."
  )

  if (
    !is.null(group_col)
  ) {
    assertthat::assert_that(
      !anyNA(data_source[[group_col]]),
      msg = "Spatial grouping values cannot be missing."
    )
  }

  vec_group <-
    if (
      is.null(group_col)
    ) {
      rep("all", nrow(data_source))
    } else {
      as.character(data_source[[group_col]])
    }

  res_coordinates <-
    data_source |>
    dplyr::mutate(
      spatial_location_id = stringr::str_c(
        vec_group,
        sprintf("%.10f", .data[[long_col]]),
        sprintf("%.10f", .data[[lat_col]]),
        sep = "|"
      )
    ) |>
    dplyr::add_count(
      .data[["spatial_location_id"]],
      name = "n_colocated"
    )

  return(res_coordinates)
}
