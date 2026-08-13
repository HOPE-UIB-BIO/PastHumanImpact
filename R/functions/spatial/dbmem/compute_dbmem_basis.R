#' @title Build grouped distance-based Moran eigenvectors
#' @description
#' Build positive dbMEM predictors independently within spatial groups and
#' combine them into a block-diagonal basis. Co-located records share values.
#' @param data_source Data frame containing identifiers and coordinates.
#' @param id_col Name of the unique record identifier column.
#' @param long_col Name of the longitude column.
#' @param lat_col Name of the latitude column.
#' @param group_col Optional grouping column. `NULL` fits one spatial network.
#' @param min_unique_locations Minimum unique locations required per network.
#' @return
#' A list with `basis`, group-level `diagnostics`, and `thresholds` in km.
#' @examples
#' \dontrun{
#' compute_dbmem_basis(
#'   data_source = metadata,
#'   group_col = "region"
#' )
#' }
compute_dbmem_basis <- function(
  data_source,
  id_col = "dataset_id",
  long_col = "long",
  lat_col = "lat",
  group_col = NULL,
  min_unique_locations = 20L
) {
  assertthat::assert_that(
    is.numeric(min_unique_locations),
    length(min_unique_locations) == 1L,
    is.finite(min_unique_locations),
    min_unique_locations >= 3L,
    msg = "`min_unique_locations` must be one number of at least three."
  )

  data_coordinates <-
    validate_spatial_coordinates(
      data_source = data_source,
      id_col = id_col,
      long_col = long_col,
      lat_col = lat_col,
      group_col = group_col
    )
  vec_spatial_group <-
    if (
      is.null(group_col)
    ) {
      rep("all", nrow(data_coordinates))
    } else {
      as.character(data_coordinates[[group_col]])
    }
  data_coordinates <-
    data_coordinates |>
    dplyr::mutate(
      .spatial_group = vec_spatial_group,
      .spatial_row_index = dplyr::row_number()
    )

  vec_groups <- unique(data_coordinates[[".spatial_group"]])
  list_groups <-
    vec_groups |>
    purrr::map(
      .f = ~ data_coordinates |>
        dplyr::filter(.data[[".spatial_group"]] == .x)
    )
  list_results <-
    purrr::map2(
      .x = list_groups,
      .y = vec_groups,
      .f = ~ build_dbmem_group_basis(
        data_group = .x,
        group_value = .y,
        long_col = long_col,
        lat_col = lat_col,
        min_unique_locations = min_unique_locations
      )
    )
  vec_mem_counts <-
    list_results |>
    purrr::map_int(.f = ~ ncol(.x[["basis"]]))
  vec_first_indices <-
    cumsum(c(1L, utils::head(vec_mem_counts, -1L)))
  list_padded <-
    purrr::pmap(
      .l = list(
        result = list_results,
        group = list_groups,
        first_index = vec_first_indices
      ),
      .f = ~ build_padded_dbmem_group_basis(
        group_basis = ..1[["basis"]],
        row_indices = ..2[[".spatial_row_index"]],
        n_records = nrow(data_coordinates),
        first_mem_index = ..3
      )
    )
  mat_basis <-
    if (
      sum(vec_mem_counts) == 0L
    ) {
      matrix(numeric(), nrow = nrow(data_coordinates), ncol = 0L)
    } else {
      purrr::reduce(list_padded, .f = cbind)
    }

  data_basis_ids <-
    data_coordinates |>
    dplyr::select(
      dplyr::all_of(c(id_col, group_col, "spatial_location_id"))
    )
  data_basis <-
    dplyr::bind_cols(
      data_basis_ids,
      tibble::as_tibble(mat_basis)
    )

  data_diagnostics <-
    list_results |>
    purrr::map(.f = ~ .x[["diagnostic"]]) |>
    dplyr::bind_rows()

  res_dbmem <-
    list(
      basis = data_basis,
      diagnostics = data_diagnostics,
      thresholds = data_diagnostics |>
        dplyr::select(
          dplyr::all_of(c("spatial_group", "threshold_km"))
        )
    )

  return(res_dbmem)
}
