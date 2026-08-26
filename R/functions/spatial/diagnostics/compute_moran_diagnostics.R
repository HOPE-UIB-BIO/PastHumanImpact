#' @title Diagnose spatial autocorrelation at fixed distance scales
#' @description
#' Calculate Moran's I with binary distance weights and reproducible one-sided
#' permutation tests. Permutations can be restricted within supplied blocks.
#' @param data_source Data frame containing coordinates and response columns.
#' @param value_cols Character vector of numeric columns to diagnose.
#' @param distance_km Positive distance thresholds in kilometres.
#' @param id_col Name of the identifier column.
#' @param long_col Name of the longitude column.
#' @param lat_col Name of the latitude column.
#' @param block_col Optional column restricting permutations within blocks.
#' @param permutations Number of random permutations.
#' @param seed Integer random seed.
#' @return One row per value and distance threshold with Moran diagnostics.
#' @examples
#' \dontrun{
#' compute_moran_diagnostics(
#'   data_source = spatial_values,
#'   value_cols = "importance_balance",
#'   distance_km = c(250, 500)
#' )
#' }
compute_moran_diagnostics <- function(
  data_source,
  value_cols,
  distance_km = c(250, 500),
  id_col = "dataset_id",
  long_col = "long",
  lat_col = "lat",
  block_col = NULL,
  permutations = 999L,
  seed = 1234L
) {
  required_columns <- c(
    id_col,
    long_col,
    lat_col,
    value_cols,
    block_col
  )
  assertthat::assert_that(
    is.data.frame(data_source),
    is.character(value_cols),
    length(value_cols) > 0L,
    all(required_columns %in% names(data_source)),
    all(purrr::map_lgl(data_source[value_cols], is.numeric)),
    all(is.finite(unlist(data_source[value_cols]))),
    is.numeric(distance_km),
    length(distance_km) > 0L,
    all(is.finite(distance_km)),
    all(distance_km > 0),
    is.numeric(permutations),
    length(permutations) == 1L,
    permutations >= 1L,
    is.numeric(seed),
    length(seed) == 1L,
    msg = "Moran diagnostic inputs do not satisfy the required contract."
  )

  data_coordinates <-
    validate_spatial_coordinates(
      data_source = data_source,
      id_col = id_col,
      long_col = long_col,
      lat_col = lat_col
    )
  mat_distance <-
    compute_spatial_distance_matrix(
      data_source = data_coordinates,
      id_col = id_col,
      long_col = long_col,
      lat_col = lat_col
    )
  vec_blocks <-
    if (
      is.null(block_col)
    ) {
      rep("all", nrow(data_source))
    } else {
      as.character(data_source[[block_col]])
    }

  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv)
  if (
    old_seed_exists
  ) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv)
  }
  on.exit(
    {
      if (
        old_seed_exists
      ) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (
        exists(".Random.seed", envir = .GlobalEnv)
      ) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  set.seed(seed)

  data_combinations <-
    tidyr::expand_grid(
      distance_km = distance_km,
      value_col = value_cols
    )
  res_diagnostics <-
    data_combinations |>
    purrr::pmap(
      .f = ~ compute_moran_scale_diagnostic(
        data_source = data_source,
        value_col = ..2,
        distance_matrix = mat_distance,
        distance_km = ..1,
        blocks = vec_blocks,
        permutations = permutations
      )
    ) |>
    dplyr::bind_rows()

  return(res_diagnostics)
}
