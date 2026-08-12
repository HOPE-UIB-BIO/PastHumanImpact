#' @title Build a dbMEM basis for one spatial group
#' @description
#' Construct positive dbMEM predictors from unique locations in one spatial
#' group and expand those predictors back to all co-located records.
#' @param data_group Validated coordinates for one spatial group.
#' @param group_value Character spatial-group label.
#' @param long_col Longitude column name.
#' @param lat_col Latitude column name.
#' @param min_unique_locations Minimum unique locations required.
#' @return A list containing a record-level matrix and one diagnostic row.
#' @examples
#' \dontrun{
#' build_dbmem_group_basis(
#'   data_group = validated_coordinates,
#'   group_value = "Europe",
#'   long_col = "long",
#'   lat_col = "lat",
#'   min_unique_locations = 20L
#' )
#' }
build_dbmem_group_basis <- function(
  data_group,
  group_value,
  long_col,
  lat_col,
  min_unique_locations
) {
  assertthat::assert_that(
    is.data.frame(data_group),
    all(c(
      "spatial_location_id",
      "n_colocated",
      long_col,
      lat_col
    ) %in% names(data_group)),
    assertthat::is.string(group_value),
    assertthat::is.string(long_col),
    assertthat::is.string(lat_col),
    is.numeric(min_unique_locations),
    length(min_unique_locations) == 1L,
    min_unique_locations >= 3L,
    msg = "Grouped dbMEM inputs do not satisfy the required contract."
  )

  data_locations <-
    data_group |>
    dplyr::distinct(
      .data[["spatial_location_id"]],
      .keep_all = TRUE
    )
  n_locations <- nrow(data_locations)

  if (
    n_locations < min_unique_locations
  ) {
    res_group <-
      list(
        basis = matrix(
          numeric(),
          nrow = nrow(data_group),
          ncol = 0L
        ),
        diagnostic = tibble::tibble(
          spatial_group = group_value,
          n_records = nrow(data_group),
          n_unique_locations = n_locations,
          n_colocated_records = sum(data_group[["n_colocated"]] > 1L),
          n_positive_mem = 0L,
          threshold_km = NA_real_,
          warning_message = NA_character_,
          status = "insufficient_locations"
        )
      )

    return(res_group)
  }

  mat_distance <-
    get_spatial_distance_matrix(
      data_source = data_locations,
      id_col = "spatial_location_id",
      long_col = long_col,
      lat_col = lat_col
    )
  result_threshold <-
    purrr::safely(
      .f = purrr::quietly(adespatial::give.thresh)
    )(
      matdist = mat_distance
    )

  if (
    !is.null(result_threshold[["error"]])
  ) {
    threshold_km <- NA_real_
    mat_record_mem <-
      matrix(numeric(), nrow = nrow(data_group), ncol = 0L)
    vec_warnings <- conditionMessage(result_threshold[["error"]])
    status <- "dbmem_error"
  } else {
    threshold_km <-
      as.numeric(result_threshold[["result"]][["result"]])
    result_dbmem <-
      purrr::safely(
        .f = purrr::quietly(adespatial::dbmem)
      )(
        xyORdist = stats::as.dist(mat_distance),
        thresh = threshold_km,
        MEM.autocor = "positive",
        store.listw = FALSE,
        silent = TRUE
      )
    vec_warnings <-
      c(
        result_threshold[["result"]][["warnings"]],
        result_dbmem[["result"]][["warnings"]]
      )

    if (
      !is.null(result_dbmem[["error"]])
    ) {
      mat_record_mem <-
        matrix(numeric(), nrow = nrow(data_group), ncol = 0L)
      vec_warnings <-
        c(vec_warnings, conditionMessage(result_dbmem[["error"]]))
      status <- "dbmem_error"
    } else {
      mat_group_mem <-
        result_dbmem[["result"]][["result"]] |>
        as.matrix()
      vec_location_match <-
        match(
          data_group[["spatial_location_id"]],
          data_locations[["spatial_location_id"]]
        )
      mat_record_mem <-
        mat_group_mem[vec_location_match, , drop = FALSE]
      status <-
        if (
          ncol(mat_record_mem) == 0L
        ) {
          "no_positive_mem"
        } else {
          "eligible"
        }
    }
  }

  warning_message <-
    if (
      length(vec_warnings) == 0L
    ) {
      NA_character_
    } else {
      stringr::str_c(unique(vec_warnings), collapse = " | ")
    }
  res_group <-
    list(
      basis = mat_record_mem,
      diagnostic = tibble::tibble(
        spatial_group = group_value,
        n_records = nrow(data_group),
        n_unique_locations = n_locations,
        n_colocated_records = sum(data_group[["n_colocated"]] > 1L),
        n_positive_mem = ncol(mat_record_mem),
        threshold_km = threshold_km,
        warning_message = warning_message,
        status = status
      )
    )

  return(res_group)
}
